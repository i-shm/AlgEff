namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Pure log handler.
type PureLogHandler<'env, 'ret when 'env :> LogContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, List<string>>()

    /// Start with an empty log.
    override _.Start = []

    /// Adds a string to the log.
    override _.TryStep(log, effect, cont) =
        Handler.tryStep effect (fun (logEff : LogEffect<_>) ->
            let log' = logEff.String :: log
            let next = logEff.Cont()
            cont log' next)

    /// Puts the log in chronological order.
    override _.Finish(log) = List.rev log

    /// Handles LogEffect.
    override _.HandledEffectTypes = [ typeof<LogEffect<Program<'env, 'ret>>> ]
