namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Effect handler base class.
///
/// Handler is the main seam of AlgEff: concrete handlers recognize effects in
/// TryStep*, while the base class owns Program traversal, scoped errors/finally,
/// async awaiting, branch materialization, and final state projection.
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
    ///
    /// This is the semantic reference path: it preserves async suspension and
    /// materializes every branch as a HandlerOutcome before Finish is applied.
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
    ///
    /// RunMany first tries the allocation-light plain-result loop. If a handler
    /// only implements the outcome-based or async hook, the loop falls back
    /// locally so old handlers keep their behavior.
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
    ///
    /// This path avoids allocating the full result list for deterministic
    /// programs. Multi-shot effects report ManyResults so callers get a precise
    /// error instead of silently receiving an arbitrary branch.
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
