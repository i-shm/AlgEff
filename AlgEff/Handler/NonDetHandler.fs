namespace AlgEff.Handler

open System
open AlgEff.Effect

[<AbstractClass>]
type NonDetHandler<'env, 'ret>() =
    inherit SimpleHandler<'env, 'ret, NoState>()

/// Always picks the true choice.
type PickTrue<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit NonDetHandler<'env, 'ret>()

    override _.Start = NoState

    override _.TryStep(NoState, effect, cont) =
        Handler.tryStep effect (fun (nonDetEff : NonDetEffect<_>) ->
            match nonDetEff.Case with
                | Decide eff ->
                    cont NoState (eff.Cont(true))
                | Fail _ -> async { return [] })

    override _.HandledEffectTypes =
        [ typeof<DecideEffect<Program<'env, 'ret>>>
          typeof<FailEffect<Program<'env, 'ret>>> ]

/// Picks the choice with the maximum value.
type PickMax<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret> and 'ret : comparison>(env : 'env) =
    inherit NonDetHandler<'env, 'ret>()

    override _.Start = NoState

    override _.TryStep(NoState, effect, cont) =
        Handler.tryStep effect (fun (nonDetEff : NonDetEffect<_>) ->
            match nonDetEff.Case with
                | Decide eff ->
                    async {
                        let! pairsTrue = cont NoState (eff.Cont(true))
                        let! pairsFalse = cont NoState (eff.Cont(false))
                        let all = pairsTrue @ pairsFalse
                        return
                            match all with
                                | [] -> []
                                | _ -> [ List.maxBy fst all ]
                    }
                | Fail _ -> async { return [] })

    override _.HandledEffectTypes =
        [ typeof<DecideEffect<Program<'env, 'ret>>>
          typeof<FailEffect<Program<'env, 'ret>>> ]

/// Picks all the choices.
type PickAll<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit NonDetHandler<'env, 'ret>()

    override _.Start = NoState

    override _.TryStep(NoState, effect, cont) =
        Handler.tryStep effect (fun (nonDetEff : NonDetEffect<_>) ->
            match nonDetEff.Case with
                | Decide eff ->
                    async {
                        let! pairsTrue = cont NoState (eff.Cont(true))
                        let! pairsFalse = cont NoState (eff.Cont(false))
                        return pairsTrue @ pairsFalse
                    }
                | Fail _ -> async { return [] })

    override _.HandledEffectTypes =
        [ typeof<DecideEffect<Program<'env, 'ret>>>
          typeof<FailEffect<Program<'env, 'ret>>> ]

module NonDetHandler =

    /// Always picks the true choice.
    let pickTrue env = PickTrue(env) :> NonDetHandler<_, _>

    /// Picks the choice with the maximum value.
    let pickMax env = PickMax(env) :> NonDetHandler<_, _>

    /// Picks all the choices.
    let pickAll env = PickAll(env) :> NonDetHandler<_, _>
