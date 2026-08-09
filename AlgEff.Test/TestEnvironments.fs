namespace AlgEff.Test

open AlgEff.Effect
open AlgEff.Handler

/// 单一 State handler 的环境。
type StateEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()
    let handler = PureStateHandler(initial, this)
    interface StateContext<'state>
    member _.Handler = handler

/// 单一 Log handler 的环境。
type LogEnv<'ret>() as this =
    inherit Environment<'ret>()
    let handler = PureLogHandler(this)
    interface LogContext
    member _.Handler = handler

/// State + Log 双 handler 环境。
type StateLogEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()
    let handler =
        Handler.combine2
            (PureStateHandler(initial, this))
            (PureLogHandler(this))
    interface StateContext<'state>
    interface LogContext
    member _.Handler = handler
