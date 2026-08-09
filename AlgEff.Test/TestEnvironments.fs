namespace AlgEff.Test

open AlgEff.Effect
open AlgEff.Handler

/// Environment with a single State handler.
type StateEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()
    let handler = PureStateHandler(initial, this)
    interface StateContext<'state>
    member _.Handler = handler

/// Environment with a single Log handler.
type LogEnv<'ret>() as this =
    inherit Environment<'ret>()
    let handler = PureLogHandler(this)
    interface LogContext
    member _.Handler = handler

/// Environment with both a State and a Log handler.
type StateLogEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()
    let handler =
        Handler.combine2
            (PureStateHandler(initial, this))
            (PureLogHandler(this))
    interface StateContext<'state>
    interface LogContext
    member _.Handler = handler

/// Declares LogContext but has no Log handler (creates a "missing handler" scenario).
type StateOnlyWithLogContextEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()
    let handler = PureStateHandler(initial, this)
    interface StateContext<'state>
    interface LogContext
    member _.Handler = handler

/// Environment defined with the HandlerEnvironment base class (contrasting StateEnv's as-this boilerplate).
type StateEnvWithBase<'state, 'ret>(initial : 'state) =
    inherit HandlerEnvironment<StateEnvWithBase<'state, 'ret>, 'ret, 'state, 'state>()
    override this.BuildHandler = PureStateHandler(initial, this)
    interface StateContext<'state>
