namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Pure state handler.
type PureStateHandler<'state, 'env, 'ret when 'env :> StateContext<'state> and 'env :> Environment<'ret>>(initial : 'state, env : 'env) =
    inherit SimpleHandler<'env, 'ret, 'state>()

    /// Start with given initial state.
    override _.Start = initial

    /// Sets or gets the state.
    override _.TryStep(state, effect, cont) =
        Handler.tryStep effect (fun (stateEff : StateEffect<_, _>) ->
            match stateEff.Case with
                | Put eff ->
                    let state' = eff.Value
                    let next = eff.Cont()
                    cont.Continue state' next
                | Get eff ->
                    let next = eff.Cont(state)
                    cont.Continue state next)

    override _.TryStepSync(state, effect, cont) =
        Handler.tryStep effect (fun (stateEff : StateEffect<_, _>) ->
            match stateEff.Case with
                | Put eff ->
                    let state' = eff.Value
                    let next = eff.Cont()
                    cont.Continue state' next
                | Get eff ->
                    let next = eff.Cont(state)
                    cont.Continue state next)

    override _.TryStepManySync(state, effect, cont) =
        match effect with
            | :? StateEffect<'state, Program<'env, 'retx>> as stateEff ->
                match stateEff.Case with
                    | Put eff ->
                        let state' = eff.Value
                        let next = eff.Cont()
                        cont.ContinueTail state' next
                    | Get eff ->
                        let next = eff.Cont(state)
                        cont.ContinueTail state next
                |> Some
            | _ -> None

    override _.TryStepOneSync(state, effect, cont) =
        match effect with
            | :? StateEffect<'state, Program<'env, 'retx>> as stateEff ->
                match stateEff.Case with
                    | Put eff ->
                        let state' = eff.Value
                        let next = eff.Cont()
                        cont.Continue state' next
                    | Get eff ->
                        let next = eff.Cont(state)
                        cont.Continue state next
                |> Some
            | _ -> None

    /// Handles Put and Get effects.
    override _.HandledEffectTypes =
        [ typeof<PutEffect<'state, Program<'env, 'ret>>>
          typeof<GetEffect<'state, Program<'env, 'ret>>> ]
