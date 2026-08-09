namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open System

open AlgEff.Effect
open AlgEff.Handler

[<TestClass>]
type EffectCETest() =

    [<TestMethod>]
    member _.While() =
        let counter = ref 0
        let program : Program<StateEnv<int, unit>, unit> =
            effect {
                while !counter < 3 do
                    do! State.put !counter
                    counter := !counter + 1
            }
        let (), finalState = StateEnv<int, unit>(0).Handler.Run(program)
        Assert.AreEqual(2, finalState)
        Assert.AreEqual(3, !counter)

    [<TestMethod>]
    member _.For() =
        let program : Program<StateEnv<int, unit>, unit> =
            effect {
                for i in [ 1; 2; 3 ] do
                    do! State.put i
            }
        let (), finalState = StateEnv<int, unit>(0).Handler.Run(program)
        Assert.AreEqual(3, finalState)

    [<TestMethod>]
    member _.TryWith() =
        let program =
            effect {
                try
                    do! (Delay (fun () -> raise (System.InvalidOperationException "boom")) : Program<_, unit>)
                    return 1
                with e ->
                    return 2
            }
        let result, _ = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(2, result)

    [<TestMethod>]
    member _.TryFinallyRunsCleanupOnSuccess() =
        let builder = ProgramBuilder()
        let program =
            builder.TryFinally(
                builder.Bind(Log.write "try", fun () -> builder.Zero()),
                Log.write "finally")
        let (), log = LogEnv().Handler.Run(program)
        Assert.AreEqual([ "try"; "finally" ], log)

    [<TestMethod>]
    member _.TryFinallyReraisesOnError() =
        let builder = ProgramBuilder()
        let program =
            builder.TryFinally(
                builder.Bind((Delay (fun () -> raise (InvalidOperationException "boom")) : Program<_, unit>), fun () -> builder.Zero()),
                Log.write "cleanup")
        Assert.Throws<InvalidOperationException>(fun () ->
            LogEnv().Handler.Run(program) |> ignore)
        |> ignore

    [<TestMethod>]
    member _.UsingDisposesOnSuccess() =
        let disposed = ref 0
        let resource =
            { new System.IDisposable with
                member _.Dispose() = disposed := !disposed + 1 }
        let program =
            effect {
                use _ = resource
                do! State.put 42
            }
        let (), finalState = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(42, finalState)
        Assert.AreEqual(1, !disposed)

    [<TestMethod>]
    member _.UsingDisposesOnError() =
        let disposed = ref 0
        let resource =
            { new System.IDisposable with
                member _.Dispose() = disposed := !disposed + 1 }
        let program =
            effect {
                use _ = resource
                do! (Delay (fun () -> raise (System.InvalidOperationException "boom")) : Program<_, unit>)
            }
        Assert.Throws<InvalidOperationException>(fun () ->
            StateEnv(0).Handler.Run(program) |> ignore)
        |> ignore
        Assert.AreEqual(1, !disposed)

    [<TestMethod>]
    member _.AsyncBind() =
        let program =
            effect {
                let! x = async { return 40 }
                do! State.put (x + 2)
                return x + 2
            }
        let result, finalState = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(42, result)
        Assert.AreEqual(42, finalState)
