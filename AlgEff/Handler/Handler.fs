namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Exception raised when an effect is not handled.
type UnhandledEffectException(effect : obj) =
    inherit System.Exception(sprintf "Unhandled effect %O" effect)
    member _.Effect = effect

/// Console input exhausted.
exception NoMoreInputException

/// A single branch produced by the run loop.
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

type private ProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> Async<List<HandlerOutcome<'ret, 'st>>>

type private SyncProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> List<HandlerOutcome<'ret, 'st>>

type private ManyProgramRunner<'ctx, 'st> =
    abstract member Run<'ret> : 'st * Program<'ctx, 'ret> -> List<'ret * 'st>

type private SingleProgramRunner<'ctx, 'st> =
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

/// Effect handler base class.
/// 'ctx: Context type requirement satisfied by this handler.
/// 'ret: Return type of program handled by this handler.
/// 'st:  Internal state type maintained by this handler.
/// 'fin: Final state type produced by this handler.
[<AbstractClass>]
type Handler<'ctx, 'ret, 'st, 'fin>() =

    /// Handler's initial state.
    abstract member Start : 'st

    /// Attempts to handle a single effect in a program.
    /// Returns None if the effect is not handled by this handler.
    /// 'cont' continues the program with the handler's new state.
    abstract member TryStep<'retx, 'stx> :
        'st                                        // state before handling current effect
            * Effect<'ctx, 'retx>                  // effect to be handled
            * HandlerCont<'ctx, 'retx, 'st, 'stx>  // continuation that will handle the remainder of the program
            -> Option<Async<List<HandlerOutcome<'retx, 'stx>>>>    // "Some" indicates the effect was handled

    /// Synchronous step hook used by Run/RunMany. Handlers that cannot run
    /// synchronously can keep the default and will be executed through RunManyAsync.
    abstract member TryStepSync<'retx, 'stx> :
        'st
            * Effect<'ctx, 'retx>
            * SyncHandlerCont<'ctx, 'retx, 'st, 'stx>
            -> Option<List<HandlerOutcome<'retx, 'stx>>>
    default _.TryStepSync(_, _, _) = None

    /// Plain synchronous step hook used by RunMany's hot path. Handlers should
    /// implement this only when they can express their normal control flow as
    /// completed result/state pairs; scoped exception/finally semantics remain
    /// handled by the run loop.
    abstract member TryStepManySync<'retx, 'stx> :
        'st
            * Effect<'ctx, 'retx>
            * ManyHandlerCont<'ctx, 'retx, 'st, 'stx>
            -> Option<HandlerManyResult<'ctx, 'retx, 'stx>>
    default _.TryStepManySync(_, _, _) = None

    /// Single-result synchronous step hook used by Run. Handlers that may
    /// produce multiple results should return ManyResults through the continuation.
    abstract member TryStepOneSync<'retx, 'stx> :
        'st
            * Effect<'ctx, 'retx>
            * SingleHandlerCont<'ctx, 'retx, 'st, 'stx>
            -> Option<HandlerRunResult<'ctx, 'retx, 'stx>>
    default _.TryStepOneSync(_, _, _) = None

    /// Transforms the handler's final state.
    abstract member Finish : 'st -> 'fin

    /// Registry of effect types (for the dispatch table; open generic definitions or concrete types).
    abstract member HandledEffectTypes : Type list

    /// Runs the given program asynchronously, producing a list of results.
    member this.RunManyAsync(program) : Async<List<'ret * 'fin>> =

        let runCompensation compensation =
            try
                compensation ()
                None
            with e ->
                Some e

        let runner =
            { new ProgramRunner<'ctx, 'st> with
                member runner.Run(state : 'st, program : Program<'ctx, 'retx>) : Async<List<HandlerOutcome<'retx, 'st>>> =
                    let continueObject (state : 'st) (next : obj) : Async<List<HandlerOutcome<'retx, 'st>>> =
                        runner.Run(state, next :?> Program<'ctx, 'retx>)

                    let runCatch (entryState : 'st) (catchEffect : ICatchEffect<'ctx>) : Async<List<HandlerOutcome<'retx, 'st>>> =
                        async {
                            let! bodyResults = runner.Run(entryState, catchEffect.BodyObject)
                            let mutable acc : List<HandlerOutcome<'retx, 'st>> = []
                            for outcome in bodyResults do
                                match outcome with
                                    | Completed(value, state') ->
                                        let! continued = continueObject state' (catchEffect.ContinueObject value)
                                        acc <- acc @ continued
                                    | Raised error ->
                                        let! handledResults = runner.Run(entryState, catchEffect.HandlerObject error)
                                        for handled in handledResults do
                                            match handled with
                                                | Completed(value, state') ->
                                                    let! continued = continueObject state' (catchEffect.ContinueObject value)
                                                    acc <- acc @ continued
                                                | Raised e -> acc <- acc @ [ Raised e ]
                                                | Aborted state' -> acc <- acc @ [ Aborted state' ]
                                    | Aborted state' ->
                                        acc <- acc @ [ Aborted state' ]
                            return acc
                        }

                    let runFinally (entryState : 'st) (finallyEffect : IFinallyEffect<'ctx>) : Async<List<HandlerOutcome<'retx, 'st>>> =
                        let completed = ref false
                        async {
                            try
                                let! bodyResults = runner.Run(entryState, finallyEffect.BodyObject)
                                let mutable acc : List<HandlerOutcome<'retx, 'st>> = []

                                for outcome in bodyResults do
                                    let! next =
                                        async {
                                            match runCompensation finallyEffect.CompensationAction with
                                                | Some error -> return [ Raised error ]
                                                | None ->
                                                    match outcome with
                                                        | Completed(value, state') ->
                                                            return! continueObject state' (finallyEffect.ContinueObject value)
                                                        | Raised error -> return [ Raised error ]
                                                        | Aborted state' -> return [ Aborted state' ]
                                        }
                                    acc <- acc @ next

                                if List.isEmpty bodyResults then
                                    match runCompensation finallyEffect.CompensationAction with
                                        | Some error -> acc <- acc @ [ Raised error ]
                                        | None -> ()

                                completed.Value <- true
                                return acc
                            finally
                                if not completed.Value then
                                    finallyEffect.CompensationAction()
                        }

                    async {
                        try
                            match program with
                                | Pure ret -> return [ Completed(ret, state) ]
                                | Effect effect ->
                                    match box effect with
                                        | :? ICatchEffect<'ctx> as catchEffect ->
                                            return! runCatch state catchEffect
                                        | :? IFinallyEffect<'ctx> as finallyEffect ->
                                            return! runFinally state finallyEffect
                                        | _ ->
                                            let cont : HandlerCont<'ctx, 'retx, 'st, 'st> =
                                                {
                                                    Continue = fun state program -> runner.Run(state, program)
                                                    Abort = fun state -> async { return [ Aborted state ] }
                                                }
                                            match this.TryStep(state, effect, cont) with
                                                | Some computation -> return! computation
                                                | None -> return [ Raised (UnhandledEffectException(effect)) ]
                                | Delay f -> return! runner.Run(state, f ())
                                | Await node ->
                                    let! value = node.Computation
                                    return! runner.Run(state, node.Continuation value)
                        with e ->
                            return [ Raised e ]
                    }
            }

        async {
            let! results = runner.Run(this.Start, program)
            return
                results
                |> List.choose (function
                    | Completed(ret, state) -> Some(ret, this.Finish(state))
                    | Aborted _ -> None
                    | Raised error -> raise error)
        }

    /// Runs the given program, producing a list of results.
    member this.RunMany(program) : List<'ret * 'fin> =
        let runCompensation compensation =
            try
                compensation ()
                None
            with e ->
                Some e

        let outcomeRunner =
            { new SyncProgramRunner<'ctx, 'st> with
                member runner.Run(state : 'st, program : Program<'ctx, 'retx>) : List<HandlerOutcome<'retx, 'st>> =
                    let continueObject (state : 'st) (next : obj) : List<HandlerOutcome<'retx, 'st>> =
                        runner.Run(state, next :?> Program<'ctx, 'retx>)

                    let runCatch (entryState : 'st) (catchEffect : ICatchEffect<'ctx>) : List<HandlerOutcome<'retx, 'st>> =
                        let bodyResults = runner.Run(entryState, catchEffect.BodyObject)
                        let acc = ResizeArray<HandlerOutcome<'retx, 'st>>()

                        for outcome in bodyResults do
                            match outcome with
                                | Completed(value, state') ->
                                    acc.AddRange(continueObject state' (catchEffect.ContinueObject value))
                                | Raised error ->
                                    let handledResults = runner.Run(entryState, catchEffect.HandlerObject error)
                                    for handled in handledResults do
                                        match handled with
                                            | Completed(value, state') ->
                                                acc.AddRange(continueObject state' (catchEffect.ContinueObject value))
                                            | Raised e -> acc.Add(Raised e)
                                            | Aborted state' -> acc.Add(Aborted state')
                                | Aborted state' ->
                                    acc.Add(Aborted state')

                        List.ofSeq acc

                    let runFinally (entryState : 'st) (finallyEffect : IFinallyEffect<'ctx>) : List<HandlerOutcome<'retx, 'st>> =
                        let completed = ref false
                        try
                            let bodyResults = runner.Run(entryState, finallyEffect.BodyObject)
                            let acc = ResizeArray<HandlerOutcome<'retx, 'st>>()

                            for outcome in bodyResults do
                                match runCompensation finallyEffect.CompensationAction with
                                    | Some error -> acc.Add(Raised error)
                                    | None ->
                                        match outcome with
                                            | Completed(value, state') ->
                                                acc.AddRange(continueObject state' (finallyEffect.ContinueObject value))
                                            | Raised error -> acc.Add(Raised error)
                                            | Aborted state' -> acc.Add(Aborted state')

                            if List.isEmpty bodyResults then
                                match runCompensation finallyEffect.CompensationAction with
                                    | Some error -> acc.Add(Raised error)
                                    | None -> ()

                            completed.Value <- true
                            List.ofSeq acc
                        finally
                            if not completed.Value then
                                finallyEffect.CompensationAction()

                    try
                        match program with
                            | Pure ret -> [ Completed(ret, state) ]
                            | Effect effect ->
                                match box effect with
                                    | :? ICatchEffect<'ctx> as catchEffect ->
                                        runCatch state catchEffect
                                    | :? IFinallyEffect<'ctx> as finallyEffect ->
                                        runFinally state finallyEffect
                                    | _ ->
                                        let cont : SyncHandlerCont<'ctx, 'retx, 'st, 'st> =
                                            {
                                                Continue = fun state program -> runner.Run(state, program)
                                                Abort = fun state -> [ Aborted state ]
                                            }
                                        match this.TryStepSync(state, effect, cont) with
                                            | Some results -> results
                                            | None ->
                                                let asyncCont : HandlerCont<'ctx, 'retx, 'st, 'st> =
                                                    {
                                                        Continue = fun state program -> async { return runner.Run(state, program) }
                                                        Abort = fun state -> async { return [ Aborted state ] }
                                                    }
                                                match this.TryStep(state, effect, asyncCont) with
                                                    | Some computation -> Async.RunSynchronously computation
                                                    | None -> [ Raised (UnhandledEffectException(effect)) ]
                            | Delay f -> runner.Run(state, f ())
                            | Await node ->
                                async {
                                    let! value = node.Computation
                                    return runner.Run(state, node.Continuation value)
                                }
                                |> Async.RunSynchronously
                    with e ->
                        [ Raised e ]
            }

        let runner =
            { new ManyProgramRunner<'ctx, 'st> with
                member runner.Run<'retx>(state : 'st, program : Program<'ctx, 'retx>) : List<'retx * 'st> =
                    let pairsFromOutcomes (outcomes : HandlerOutcome<'retx, 'st> list) : ('retx * 'st) list =
                        outcomes
                        |> List.choose (function
                            | Completed(ret, state) -> Some(ret, state)
                            | Aborted _ -> None
                            | Raised error -> raise error)

                    let mutable state = state
                    let mutable program = program
                    let mutable result : List<'retx * 'st> option = None

                    while result.IsNone do
                        let stepResult =
                            try
                                match program with
                                    | Pure ret -> ManyCompleted [ ret, state ]
                                    | Effect effect ->
                                        let boxed = box effect
                                        if boxed :? ICatchEffect<'ctx> || boxed :? IFinallyEffect<'ctx> then
                                            outcomeRunner.Run(state, program) |> pairsFromOutcomes |> ManyCompleted
                                        else
                                            let cont : ManyHandlerCont<'ctx, 'retx, 'st, 'st> =
                                                {
                                                    Continue = fun state program -> runner.Run(state, program)
                                                    ContinueTail = fun state program -> ContinueManyWith(state, program)
                                                    Abort = fun _ -> []
                                                }
                                            match this.TryStepManySync(state, effect, cont) with
                                                | Some results -> results
                                                | None ->
                                                    outcomeRunner.Run(state, program) |> pairsFromOutcomes |> ManyCompleted
                                    | Delay f -> ContinueManyWith(state, f ())
                                    | Await node ->
                                        async {
                                            let! value = node.Computation
                                            return ContinueManyWith(state, node.Continuation value)
                                        }
                                        |> Async.RunSynchronously
                            with e ->
                                ManyRaised e

                        match stepResult with
                            | ContinueManyWith(state', program') ->
                                state <- state'
                                program <- program'
                            | ManyCompleted pairs ->
                                result <- Some pairs
                            | ManyRaised error ->
                                raise error

                    result.Value
            }

        runner.Run(this.Start, program)
        |> List.map (fun (ret, state) ->
            ret, this.Finish(state))

    /// Runs the given program, producing a single result.
    member this.Run(program) : 'ret * 'fin =
        let runCompensation compensation =
            try
                compensation ()
                None
            with e ->
                Some e

        let fromOutcomes (fallbackState : 'st) (outcomes : HandlerOutcome<'retx, 'st> list) : HandlerRunResult<'ctx, 'retx, 'st> =
            let mutable count = 0
            let mutable first = Unchecked.defaultof<'retx * 'st>
            let mutable noResultState = fallbackState
            let mutable result : HandlerRunResult<'ctx, 'retx, 'st> option = None
            for outcome in outcomes do
                match result, outcome with
                    | Some _, _ -> ()
                    | None, Completed(ret, state) ->
                        count <- count + 1
                        if count = 1 then
                            first <- ret, state
                    | None, Aborted state ->
                        noResultState <- state
                    | None, Raised error ->
                        result <- Some(RunRaised error)
            match result with
                | Some value -> value
                | None ->
                    match count with
                        | 0 -> NoResult noResultState
                        | 1 -> OneResult first
                        | _ -> ManyResults count

        let resultToOutcomes = function
            | ContinueWith _ ->
                [ Raised (InvalidOperationException("Internal error: unresolved continuation in single-result fast path")) ]
            | OneResult(ret, state) -> [ Completed(ret, state) ]
            | NoResult state -> [ Aborted state ]
            | ManyResults _ ->
                [ Raised (InvalidOperationException("Program produced multiple results; use RunMany for multi-shot programs")) ]
            | RunRaised error -> [ Raised error ]

        let runner =
            { new SingleProgramRunner<'ctx, 'st> with
                member runner.Run(initialState : 'st, initialProgram : Program<'ctx, 'retx>) : HandlerRunResult<'ctx, 'retx, 'st> =
                    let continueObject (state : 'st) (next : obj) : HandlerRunResult<'ctx, 'retx, 'st> =
                        ContinueWith(state, next :?> Program<'ctx, 'retx>)

                    let runCatch (entryState : 'st) (catchEffect : ICatchEffect<'ctx>) : HandlerRunResult<'ctx, 'retx, 'st> =
                        match runner.Run(entryState, catchEffect.BodyObject) with
                            | ContinueWith _ ->
                                RunRaised (InvalidOperationException("Internal error: unresolved continuation in catch body"))
                            | OneResult(value, state') ->
                                continueObject state' (catchEffect.ContinueObject value)
                            | RunRaised error ->
                                match runner.Run(entryState, catchEffect.HandlerObject error) with
                                    | ContinueWith _ ->
                                        RunRaised (InvalidOperationException("Internal error: unresolved continuation in catch handler"))
                                    | OneResult(value, state') ->
                                        continueObject state' (catchEffect.ContinueObject value)
                                    | NoResult state' -> NoResult state'
                                    | ManyResults count -> ManyResults count
                                    | RunRaised error -> RunRaised error
                            | NoResult state' -> NoResult state'
                            | ManyResults count -> ManyResults count

                    let runFinally (entryState : 'st) (finallyEffect : IFinallyEffect<'ctx>) : HandlerRunResult<'ctx, 'retx, 'st> =
                        let completed = ref false
                        try
                            let bodyResult = runner.Run(entryState, finallyEffect.BodyObject)
                            let result =
                                match runCompensation finallyEffect.CompensationAction with
                                    | Some error -> RunRaised error
                                    | None ->
                                        match bodyResult with
                                            | ContinueWith _ ->
                                                RunRaised (InvalidOperationException("Internal error: unresolved continuation in finally body"))
                                            | OneResult(value, state') ->
                                                continueObject state' (finallyEffect.ContinueObject value)
                                            | NoResult state' -> NoResult state'
                                            | ManyResults count -> ManyResults count
                                            | RunRaised error -> RunRaised error
                            completed.Value <- true
                            result
                        finally
                            if not completed.Value then
                                finallyEffect.CompensationAction()

                    let mutable state = initialState
                    let mutable program = initialProgram
                    let mutable result : HandlerRunResult<'ctx, 'retx, 'st> option = None
                    while result.IsNone do
                        let stepResult =
                            try
                                match program with
                                    | Pure ret -> OneResult(ret, state)
                                    | Effect effect ->
                                        match box effect with
                                            | :? ICatchEffect<'ctx> as catchEffect ->
                                                runCatch state catchEffect
                                            | :? IFinallyEffect<'ctx> as finallyEffect ->
                                                runFinally state finallyEffect
                                            | _ ->
                                                let cont : SingleHandlerCont<'ctx, 'retx, 'st, 'st> =
                                                    {
                                                        Continue = fun state program -> ContinueWith(state, program)
                                                        Abort = fun state -> NoResult state
                                                    }
                                                match this.TryStepOneSync(state, effect, cont) with
                                                    | Some result -> result
                                                    | None ->
                                                        let syncCont : SyncHandlerCont<'ctx, 'retx, 'st, 'st> =
                                                            {
                                                                Continue = fun state program -> resultToOutcomes (runner.Run(state, program))
                                                                Abort = fun state -> [ Aborted state ]
                                                            }
                                                        match this.TryStepSync(state, effect, syncCont) with
                                                            | Some outcomes -> fromOutcomes state outcomes
                                                            | None ->
                                                                let asyncCont : HandlerCont<'ctx, 'retx, 'st, 'st> =
                                                                    {
                                                                        Continue = fun state program -> async { return resultToOutcomes (runner.Run(state, program)) }
                                                                        Abort = fun state -> async { return [ Aborted state ] }
                                                                    }
                                                                match this.TryStep(state, effect, asyncCont) with
                                                                    | Some computation -> Async.RunSynchronously computation |> fromOutcomes state
                                                                    | None -> RunRaised (UnhandledEffectException(effect))
                                    | Delay f -> ContinueWith(state, f ())
                                    | Await node ->
                                        async {
                                            let! value = node.Computation
                                            return ContinueWith(state, node.Continuation value)
                                        }
                                        |> Async.RunSynchronously
                            with e ->
                                RunRaised e
                        match stepResult with
                            | ContinueWith(state', program') ->
                                state <- state'
                                program <- program'
                            | terminal ->
                                result <- Some terminal
                    result.Value
            }

        match runner.Run(this.Start, program) with
            | ContinueWith _ -> raise (InvalidOperationException("Internal error: unresolved continuation in single-result fast path"))
            | OneResult(ret, state) -> ret, this.Finish(state)
            | NoResult _ -> raise (InvalidOperationException("Program produced no results"))
            | ManyResults count ->
                raise (InvalidOperationException(
                    sprintf "Program produced %d results; use RunMany for multi-shot programs" count))
            | RunRaised error -> raise error

/// Handler whose final state type is the same as its internal state type.
[<AbstractClass>]
type SimpleHandler<'ctx, 'ret, 'st>() =
    inherit Handler<'ctx, 'ret, 'st, 'st>()

    /// No-op final transformation.
    default _.Finish(state) = state

/// An environment handler with no handlers (running any program yields an unhandled effect).
type NoopHandler<'ctx, 'ret, 'st, 'fin>(start : 'st, finish : 'st -> 'fin) =
    inherit Handler<'ctx, 'ret, 'st, 'fin>()
    override _.Start = start
    override _.TryStep(state, effect, _) = None
    override _.Finish(state) = finish state
    override _.HandledEffectTypes = []

/// Combines two effect handlers using the given finish.
/// Dispatch table: effect closed type -> sub-handler index, O(1) exact routing.
type private CombinedHandler<'ctx, 'ret, 'st1, 'fin1, 'st2, 'fin2, 'fin>
    (handler1 : Handler<'ctx, 'ret, 'st1, 'fin1>,
     handler2 : Handler<'ctx, 'ret, 'st2, 'fin2>,
     finish : ('fin1 * 'fin2) -> 'fin) =
    inherit Handler<'ctx, 'ret, 'st1 * 'st2, 'fin>()

    let table =
        let table = System.Collections.Generic.Dictionary<Type, int>()
        for t in handler1.HandledEffectTypes do
            if table.ContainsKey t then
                raise (InvalidOperationException(
                    sprintf "Effect type %O is registered by multiple handlers in this combination" t))
            table.[t] <- 0
        for t in handler2.HandledEffectTypes do
            if table.ContainsKey t then
                raise (InvalidOperationException(
                    sprintf "Effect type %O is registered by multiple handlers in this combination" t))
            table.[t] <- 1
        table

    /// Looks up a type by walking up the base-class chain (supports handlers that declare base-class types).
    let rec lookup (t : Type) =
        if isNull t then None
        else
            match table.TryGetValue t with
                | true, index -> Some index
                | _ -> lookup t.BaseType

    override _.Start = handler1.Start, handler2.Start

    /// Attempts to handle a single effect, routed via the dispatch table.
    /// If the routed handler declines (returns None), the other handler is still
    /// given a chance via the linear fallback, so a catch-all registered at an
    /// abstract base type is not shadowed by a more specific but declining handler.
    override _.TryStep<'retx, 'stx>(
        (state1, state2),
        effect : Effect<'ctx, 'retx>,
        cont : HandlerCont<'ctx, 'retx, 'st1 * 'st2, 'stx>) =
        let step1 : HandlerCont<'ctx, 'retx, 'st1, 'stx> =
            {
                Continue = fun state1' program -> cont.Continue (state1', state2) program
                Abort = fun state1' -> cont.Abort (state1', state2)
            }
        let step2 : HandlerCont<'ctx, 'retx, 'st2, 'stx> =
            {
                Continue = fun state2' program -> cont.Continue (state1, state2') program
                Abort = fun state2' -> cont.Abort (state1, state2')
            }
        let fallback () =
            handler1.TryStep(state1, effect, step1)
            |> Option.orElseWith (fun () ->
                handler2.TryStep(state2, effect, step2))
        match lookup (effect.GetType()) with
            | Some 0 ->
                match handler1.TryStep(state1, effect, step1) with
                    | Some result -> Some result
                    | None -> fallback ()
            | Some 1 ->
                match handler2.TryStep(state2, effect, step2) with
                    | Some result -> Some result
                    | None -> fallback ()
            | _ -> fallback ()

    override _.TryStepSync<'retx, 'stx>(
        (state1, state2),
        effect : Effect<'ctx, 'retx>,
        cont : SyncHandlerCont<'ctx, 'retx, 'st1 * 'st2, 'stx>) =
        let step1 : SyncHandlerCont<'ctx, 'retx, 'st1, 'stx> =
            {
                Continue = fun state1' program -> cont.Continue (state1', state2) program
                Abort = fun state1' -> cont.Abort (state1', state2)
            }
        let step2 : SyncHandlerCont<'ctx, 'retx, 'st2, 'stx> =
            {
                Continue = fun state2' program -> cont.Continue (state1, state2') program
                Abort = fun state2' -> cont.Abort (state1, state2')
            }
        let fallback () =
            handler1.TryStepSync(state1, effect, step1)
            |> Option.orElseWith (fun () ->
                handler2.TryStepSync(state2, effect, step2))
        match lookup (effect.GetType()) with
            | Some 0 ->
                match handler1.TryStepSync(state1, effect, step1) with
                    | Some result -> Some result
                    | None -> fallback ()
            | Some 1 ->
                match handler2.TryStepSync(state2, effect, step2) with
                    | Some result -> Some result
                    | None -> fallback ()
            | _ -> fallback ()

    override _.TryStepManySync<'retx, 'stx>(
        (state1, state2),
        effect : Effect<'ctx, 'retx>,
        cont : ManyHandlerCont<'ctx, 'retx, 'st1 * 'st2, 'stx>) =
        let step1 : ManyHandlerCont<'ctx, 'retx, 'st1, 'stx> =
            {
                Continue = fun state1' program -> cont.Continue (state1', state2) program
                ContinueTail = fun state1' program -> cont.ContinueTail (state1', state2) program
                Abort = fun state1' -> cont.Abort (state1', state2)
            }
        let step2 : ManyHandlerCont<'ctx, 'retx, 'st2, 'stx> =
            {
                Continue = fun state2' program -> cont.Continue (state1, state2') program
                ContinueTail = fun state2' program -> cont.ContinueTail (state1, state2') program
                Abort = fun state2' -> cont.Abort (state1, state2')
            }
        let fallback () =
            handler1.TryStepManySync(state1, effect, step1)
            |> Option.orElseWith (fun () ->
                handler2.TryStepManySync(state2, effect, step2))
        match lookup (effect.GetType()) with
            | Some 0 ->
                match handler1.TryStepManySync(state1, effect, step1) with
                    | Some result -> Some result
                    | None -> fallback ()
            | Some 1 ->
                match handler2.TryStepManySync(state2, effect, step2) with
                    | Some result -> Some result
                    | None -> fallback ()
            | _ -> fallback ()

    override _.TryStepOneSync<'retx, 'stx>(
        (state1, state2),
        effect : Effect<'ctx, 'retx>,
        cont : SingleHandlerCont<'ctx, 'retx, 'st1 * 'st2, 'stx>) =
        let step1 : SingleHandlerCont<'ctx, 'retx, 'st1, 'stx> =
            {
                Continue = fun state1' program -> cont.Continue (state1', state2) program
                Abort = fun state1' -> cont.Abort (state1', state2)
            }
        let step2 : SingleHandlerCont<'ctx, 'retx, 'st2, 'stx> =
            {
                Continue = fun state2' program -> cont.Continue (state1, state2') program
                Abort = fun state2' -> cont.Abort (state1, state2')
            }
        let fallback () =
            handler1.TryStepOneSync(state1, effect, step1)
            |> Option.orElseWith (fun () ->
                handler2.TryStepOneSync(state2, effect, step2))
        match lookup (effect.GetType()) with
            | Some 0 ->
                match handler1.TryStepOneSync(state1, effect, step1) with
                    | Some result -> Some result
                    | None -> fallback ()
            | Some 1 ->
                match handler2.TryStepOneSync(state2, effect, step2) with
                    | Some result -> Some result
                    | None -> fallback ()
            | _ -> fallback ()

    /// Combines the given handlers' final states.
    override _.Finish((state1, state2)) =
        finish (handler1.Finish(state1), handler2.Finish(state2))

    override _.HandledEffectTypes =
        handler1.HandledEffectTypes @ handler2.HandledEffectTypes

module Handler =

    /// A handler with no handlers (for testing the unhandled-effect path).
    let noopHandler<'ctx, 'ret> : Handler<'ctx, 'ret, unit, unit> =
        NoopHandler((), id) :> _

    /// Adapts a step function for use in an effect handler.
    /// Exact type matching: subclasses are accepted only when the declared type is an
    /// abstract family type (e.g. StateEffect<_>); otherwise (e.g. the concrete type
    /// LogEffect<_>) only the exact type is accepted — subclass effects are not swallowed
    /// by family handlers.
    let tryStep<'eff, 'next, 'ret when 'eff :> Effect<'next>>
        (effect : Effect<'next>)
        (step : 'eff -> 'ret) =
            let effectType = effect.GetType()
            let effType = typeof<'eff>
            let matches =
                effectType = effType
                || (effType.IsAbstract && effType.IsAssignableFrom(effectType))
            if matches then step (effect :?> 'eff) |> Some
            else None

    /// Combines two handlers using the given finish.
    let private combine handler1 handler2 finish =
        CombinedHandler(handler1, handler2, finish) :> Handler<_, _, _, _>

    /// Combines two handlers.
    let combine2 handler1 handler2 =
        combine handler1 handler2 id

    /// Combines three handlers.
    let combine3 handler1 handler2 handler3 =
        combine
            (combine2 handler1 handler2)
            handler3
            (fun ((s1, s2), s3) -> s1, s2, s3)

    /// Combines four handlers.
    let combine4 handler1 handler2 handler3 handler4 =
        combine
            (combine3 handler1 handler2 handler3)
            handler4
            (fun ((s1, s2, s3), s4) -> s1, s2, s3, s4)

    /// Combines five handlers.
    let combine5 handler1 handler2 handler3 handler4 handler5 =
        combine
            (combine4 handler1 handler2 handler3 handler4)
            handler5
            (fun ((s1, s2, s3, s4), s5) -> s1, s2, s3, s4, s5)

/// Base type for concrete classes that satisfy an effect type's context
/// requirement.
[<AbstractClass>]
type Environment<'ret>() = class end

/// State type for handlers that maintain no internal state.
/// (Replaces the former single-case-union hack that shadowed unit as a
/// type argument.)
[<Struct>]
type NoState = NoState

/// Convenience environment base class: builds and caches the combined
/// handler on first access. 'BuildHandler' is invoked on first access to
/// Handler, so its override may safely reference `this`.
[<AbstractClass>]
type HandlerEnvironment<'env, 'ret, 'st, 'fin>() =
    inherit Environment<'ret>()
    let mutable handler : Handler<'env, 'ret, 'st, 'fin> option = None

    /// Builds the combined handler on first access to Handler.
    /// The override may safely reference `this`.
    abstract member BuildHandler : Handler<'env, 'ret, 'st, 'fin>

    /// The combined handler, built once and cached for subsequent accesses.
    member this.Handler =
        match handler with
            | Some h -> h
            | None ->
                let h = this.BuildHandler
                handler <- Some h
                h
