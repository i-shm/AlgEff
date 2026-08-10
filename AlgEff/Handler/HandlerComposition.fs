namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Combines two effect handlers using the given finish.
///
/// The combined handler owns the dispatch-table invariant: no two child
/// handlers may register the same effect family, and routed handlers that
/// decline still fall back to the other child for catch-all cases.
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

/// Helper functions for composing handlers and writing small typed adapters.
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
