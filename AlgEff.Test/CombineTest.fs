namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

/// 继承 LogEffect 的子类（用于分派不误配测试）。
type MyLogEffect<'next>(str : string, cont : unit -> 'next) =
    inherit LogEffect<'next>(str, cont)

/// 4 个 handler：log + int state + string state + console。
/// int/string 两个 State 家族 handler 必须各自精确路由。
type FourEnv(initialInt : int, initialString : string, consoleInput : List<string>) as this =
    inherit Environment<unit>()
    let handler =
        Handler.combine4
            (PureLogHandler(this))
            (PureStateHandler<int, FourEnv, unit>(initialInt, this))
            (PureStateHandler<string, FourEnv, unit>(initialString, this))
            (PureConsoleHandler(consoleInput, this))
    interface LogContext
    interface StateContext<int>
    interface StateContext<string>
    interface ConsoleContext
    member _.Handler = handler

/// 5 个 handler 嵌套组合（突破 combine5 上限）。
type FiveEnv() as this =
    inherit Environment<unit>()
    let handler =
        Handler.combine2
            (Handler.combine2
                (PureLogHandler(this))
                (PureConsoleHandler([], this)))
            (Handler.combine2
                (Handler.combine2
                    (PureStateHandler<int, FiveEnv, unit>(0, this))
                    (PureStateHandler<string, FiveEnv, unit>("", this)))
                (NonDetHandler.pickTrue this))
    interface LogContext
    interface ConsoleContext
    interface StateContext<int>
    interface StateContext<string>
    interface NonDetContext
    member _.Handler = handler

/// 声明基类 Effect<_> 的 handler（通过基类链回溯命中）。
type EverythingHandler<'env, 'ret>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, bool>()
    override _.Start = false
    override _.TryStep(state, effect, cont) =
        Handler.tryStep effect (fun (logEff : LogEffect<_>) ->
            let next = logEff.Cont()
            cont true next)
    override _.HandledEffectTypes =
        [ typeof<Effect<Program<'env, 'ret>>> ]

type EverythingEnv<'ret>() as this =
    inherit Environment<'ret>()
    let handler = EverythingHandler(this)
    interface LogContext
    member _.Handler = handler

/// 基类声明 handler 与 State handler 的组合（验证分派表的基类链回溯）。
type EverythingStateEnv(initial : int) as this =
    inherit Environment<int>()
    let handler =
        Handler.combine2
            (EverythingHandler(this))
            (PureStateHandler(initial, this))
    interface LogContext
    interface StateContext<int>
    member _.Handler = handler

[<TestClass>]
type CombineTest() =

    [<TestMethod>]
    member _.SameFamilyDifferentStateTypesRouteCorrectly() =
        let program : Program<FourEnv, unit> =
            effect {
                do! Log.write "start"
                let! n = State.get
                do! State.put<int, FourEnv> (n + 1)
                do! State.put "done"
                do! Console.writeln (sprintf "n=%d" n)
                return ()
            }
        let (), (log, intState, strState, console) =
            FourEnv(1, "x", []).Handler.Run(program)
        Assert.AreEqual([ "start" ], log)
        Assert.AreEqual(2, intState)
        Assert.AreEqual("done", strState)
        Assert.AreEqual([ "n=1" ], console.Output)

    [<TestMethod>]
    member _.FiveHandlersViaNestedCombine() =
        let program : Program<FiveEnv, unit> =
            effect {
                do! Log.write "l"
                do! Console.writeln "c"
                do! State.put<int, FiveEnv> 1
                do! State.put "s"
                let! _flag = NonDet.decide
                return ()
            }
        let (), ((log, console), ((intState, strState), _noDetState)) =
            FiveEnv().Handler.Run(program)
        Assert.AreEqual([ "l" ], log)
        Assert.AreEqual([ "c" ], console.Output)
        Assert.AreEqual(1, intState)
        Assert.AreEqual("s", strState)

    [<TestMethod>]
    member _.SubclassIsNotRoutedToBaseHandler() =
        // 子类效应不被 LogEffect 家族 handler 误吞（旧 `:?` 形状测试会误吞）
        let env = LogEnv()
        let program =
            Program.Effect (MyLogEffect("sneaky", fun () -> Pure 0))
        Assert.Throws<UnhandledEffectException>(fun () ->
            env.Handler.Run(program) |> ignore)
        |> ignore

    [<TestMethod>]
    member _.CombinedBaseChainRouting() =
        let program =
            effect {
                do! Log.write "x"
                do! State.put 42
                return 1
            }
        let result, (handled, state) = EverythingStateEnv(0).Handler.Run(program)
        Assert.AreEqual(1, result)
        Assert.IsTrue(handled)
        Assert.AreEqual(42, state)

    [<TestMethod>]
    member _.BaseTypeDeclarationMatches() =
        // handler 声明基类 Effect<_> 时，具体效应通过基类链回溯命中
        let env = EverythingEnv<int>()
        let program =
            effect {
                do! Log.write "anything"
                return 1
            }
        let result, handled = env.Handler.Run(program)
        Assert.AreEqual(1, result)
        Assert.IsTrue(handled)
