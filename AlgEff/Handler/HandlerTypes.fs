namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Exception raised when an effect reaches the runner without a matching handler.
type UnhandledEffectException(effect : obj) =
    inherit Exception(sprintf "Unhandled effect %O" effect)
    member _.Effect = effect

/// Console input exhausted.
exception NoMoreInputException

/// A single branch produced by the run loop.
///
/// Completed and Raised mirror normal program exit. Aborted is the deliberate
/// "no value" path used by effects such as NonDet.fail and Concurrency.exit;
/// it must still unwind dynamic scopes such as try/finally and use.
[<Struct>]
type HandlerOutcome<'ret, 'st> =
    | Completed of Return : 'ret * State : 'st
    | Raised of RaisedError : exn
    | Aborted of State : 'st

/// Compact result used by Run's single-result fast path.
type HandlerRunResult<'ctx, 'ret, 'st> =
    | ContinueWith of 'st * Program<'ctx, 'ret>
    | OneResult of 'ret * 'st
    | NoResult of 'st
    | ManyResults of int
    | RunRaised of exn

/// Compact result used by RunMany's plain-result fast path.
type HandlerManyResult<'ctx, 'ret, 'st> =
    | ContinueManyWith of 'st * Program<'ctx, 'ret>
    | ManyCompleted of List<'ret * 'st>
    | ManyRaised of exn

// These runner adapters are implementation details of Handler's three run
// loops. They live here because continuation records refer to their result
// shapes, but handler authors should implement TryStep* rather than these.
type internal ProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> Async<List<HandlerOutcome<'ret, 'st>>>

type internal SyncProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> List<HandlerOutcome<'ret, 'st>>

type internal ManyProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> List<'ret * 'st>

type internal SingleProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> HandlerRunResult<'ctx, 'ret, 'st>

/// Continuation that handles the remainder of the program (async).
/// 'st:  State type maintained by the handler.
/// 'stx: State type answered by the continuation, which may be
///       different from the state managed by the handler (e.g. the
///       combined state of multiple handlers).
type HandlerCont<'ctx, 'ret, 'st, 'stx> =
    {
        /// Continues the current branch with a program.
        Continue : 'st -> Program<'ctx, 'ret> -> Async<List<HandlerOutcome<'ret, 'stx>>>

        /// Aborts the current branch while still unwinding dynamic scopes.
        Abort : 'st -> Async<List<HandlerOutcome<'ret, 'stx>>>
    }

/// Synchronous continuation used by Run/RunMany fast paths.
type SyncHandlerCont<'ctx, 'ret, 'st, 'stx> =
    {
        /// Continues the current branch with a program.
        Continue : 'st -> Program<'ctx, 'ret> -> List<HandlerOutcome<'ret, 'stx>>

        /// Aborts the current branch while still unwinding dynamic scopes.
        Abort : 'st -> List<HandlerOutcome<'ret, 'stx>>
    }

/// Plain synchronous continuation used by RunMany's hot path.
type ManyHandlerCont<'ctx, 'ret, 'st, 'stx> =
    {
        /// Continues and materializes the current branch with a program.
        Continue : 'st -> Program<'ctx, 'ret> -> List<'ret * 'stx>

        /// Tail-continues the current branch without growing the call stack.
        ContinueTail : 'st -> Program<'ctx, 'ret> -> HandlerManyResult<'ctx, 'ret, 'stx>

        /// Aborts the current branch while still unwinding dynamic scopes.
        Abort : 'st -> List<'ret * 'stx>
    }

/// Single-result continuation used by Run's fast path.
type SingleHandlerCont<'ctx, 'ret, 'st, 'stx> =
    {
        /// Continues the current branch with a program.
        Continue : 'st -> Program<'ctx, 'ret> -> HandlerRunResult<'ctx, 'ret, 'stx>

        /// Aborts the current branch while still unwinding dynamic scopes.
        Abort : 'st -> HandlerRunResult<'ctx, 'ret, 'stx>
    }
