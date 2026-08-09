namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open System

open AlgEff.Effect
open AlgEff.Handler

/// 没有任何 handler 的环境（制造未处理效应）。
type EmptyEnv<'ret>() =
    inherit Environment<'ret>()
    member _.Handler = Handler.noopHandler

/// 仅 NonDet(pickAll) 的环境。
type NonDetOnlyEnv<'ret>(createHandler : _ -> NonDetHandler<NonDetOnlyEnv<'ret>, 'ret>) as this =
    inherit Environment<'ret>()
    let handler = createHandler(this)
    interface NonDetContext
    member _.Handler = handler

/// 仅纯 Console 的环境。
type ConsoleOnlyEnv<'ret>(input : List<string>) as this =
    inherit Environment<'ret>()
    let handler = PureConsoleHandler(input, this)
    interface ConsoleContext
    member _.Handler = handler

[<TestClass>]
type ErrorTest() =

    [<TestMethod>]
    member _.UnhandledEffect() =
        let program =
            effect {
                do! Log.write "x"
            }
        let ex =
            Assert.Throws<UnhandledEffectException>(fun () ->
                EmptyEnv().Handler.Run(program) |> ignore)
        Assert.IsTrue(ex.Message.Contains("Log(x)"))
        Assert.IsTrue(ex.Message.Contains("Unhandled effect"))

    [<TestMethod>]
    member _.RunWithMultipleResults() =
        let program =
            effect {
                let! flag = NonDet.decide
                return flag
            }
        let env = NonDetOnlyEnv(NonDetHandler.pickAll)
        let ex =
            Assert.Throws<InvalidOperationException>(fun () ->
                env.Handler.Run(program) |> ignore)
        Assert.IsTrue(ex.Message.Contains("RunMany"))

    [<TestMethod>]
    member _.NoMoreInput() =
        let program =
            effect {
                let! _ = Console.readln
                let! _ = Console.readln
                return ()
            }
        let env = ConsoleOnlyEnv([""])
        Assert.Throws<NoMoreInputException>(fun () ->
            env.Handler.Run(program) |> ignore)
        |> ignore
