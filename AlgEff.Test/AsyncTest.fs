namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open System.Threading

open AlgEff.Effect
open AlgEff.Handler

[<TestClass>]
type AsyncTest() =

    [<TestMethod>]
    member _.AwaitInMiddleOfProgram() =
        let program =
            effect {
                do! Log.write "before"
                let! x = async { return 42 }
                do! State.put x
                return! State.get
            }
        let result, (state, log) =
            StateLogEnv(0).Handler.Run(program)
        Assert.AreEqual(42, result)
        Assert.AreEqual([ "before" ], log)
        Assert.AreEqual(42, state)

    [<TestMethod>]
    member _.RunManyAsyncAwaitsForReal() =
        let program =
            effect {
                let! x =
                    async {
                        do! Async.Sleep 50
                        return 7
                    }
                return x * 2
            }
        let result =
            StateEnv(0).Handler.RunManyAsync(program)
            |> Async.RunSynchronously
            |> List.exactlyOne
            |> fst
        Assert.AreEqual(14, result)

    [<TestMethod>]
    member _.AwaitInsideWhileLoop() =
        let counter = ref 0
        let program =
            effect {
                while !counter < 2 do
                    let! x = async { return !counter }
                    do! State.put x
                    counter := !counter + 1
            }
        let (), finalState = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(1, finalState)
        Assert.AreEqual(2, !counter)

    [<TestMethod>]
    member _.WhileBangAwaitsCondition() =
        let counter = ref 0
        let program =
            effect {
                while! async { return !counter < 3 } do
                    do! State.put !counter
                    counter := !counter + 1
            }
        let (), finalState = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(2, finalState)
        Assert.AreEqual(3, !counter)

    [<TestMethod>]
    member _.AwaitInsideNonDet() =
        let program =
            effect {
                let! x = NonDet.choose 1 2
                let! y = async { return x * 10 }
                return y
            }
        let results =
            program
                |> NonDetLogEnv(NonDetHandler.pickAll).Handler.RunMany
                |> List.map fst
        Assert.AreEqual([ 10; 20 ], results)

    [<TestMethod>]
    member _.UsingDisposesWhenRunManyAsyncIsCancelled() =
        use started = new ManualResetEventSlim(false)
        use disposed = new ManualResetEventSlim(false)
        use cts = new CancellationTokenSource()
        let resource =
            { new System.IDisposable with
                member _.Dispose() = disposed.Set() |> ignore }
        let program =
            effect {
                use _ = resource
                started.Set() |> ignore
                let! _ =
                    async {
                        do! Async.Sleep 1000000
                        return 0
                    }
                return ()
            }
        Async.StartWithContinuations(
            StateEnv(0).Handler.RunManyAsync(program),
            ignore,
            ignore,
            ignore,
            cts.Token)
        Assert.IsTrue(started.Wait 1000)
        cts.Cancel()
        Assert.IsTrue(disposed.Wait 1000)
