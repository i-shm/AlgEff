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
    member _.TryFinallyCompensationThrowsRunsOnce() =
        let runs = ref 0
        Assert.Throws<InvalidOperationException>(fun () ->
            effect {
                try
                    return 1
                finally
                    runs := !runs + 1
                    raise (System.InvalidOperationException "boom")
            }
            |> StateEnv(0).Handler.Run
            |> ignore)
        |> ignore
        Assert.AreEqual(1, !runs)

    [<TestMethod>]
    member _.TryWithCatchesUnhandledEffect() =
        let program =
            effect {
                try
                    do! Log.write "x"
                    return 0
                with _ ->
                    return 1
            }
        let result, _ = StateOnlyWithLogContextEnv(0).Handler.Run(program)
        Assert.AreEqual(1, result)

    [<TestMethod>]
    member _.CatchStateIsTryEntryState() =
        let program =
            effect {
                do! State.put 1
                try
                    do! State.put 2
                    do! (Delay (fun () -> raise (System.InvalidOperationException "boom")) : Program<_, unit>)
                    return 0
                with _ ->
                    return! State.get
            }
        let result, state = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(1, result)
        Assert.AreEqual(1, state)

    [<TestMethod>]
    member _.AwaitInsideTryFinally() =
        let cleanupRan = ref 0
        let program =
            effect {
                try
                    let! x = async { return 42 }
                    do! State.put x
                finally
                    cleanupRan := !cleanupRan + 1
            }
        let (), state = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(42, state)
        Assert.AreEqual(1, !cleanupRan)

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
        let cleanupRan = ref 0
        let program =
            effect {
                try
                    do! Log.write "try"
                finally
                    cleanupRan := !cleanupRan + 1
            }
        let (), log = LogEnv().Handler.Run(program)
        Assert.AreEqual([ "try" ], log)
        Assert.AreEqual(1, !cleanupRan)

    [<TestMethod>]
    member _.TryFinallyReraisesOnError() =
        let cleanupRan = ref 0
        let program =
            effect {
                try
                    do! (Delay (fun () -> raise (InvalidOperationException "boom")) : Program<_, unit>)
                finally
                    cleanupRan := !cleanupRan + 1
            }
        Assert.Throws<InvalidOperationException>(fun () ->
            StateEnv(0).Handler.Run(program)
            |> ignore)
        |> ignore
        Assert.AreEqual(1, !cleanupRan)

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
    member _.UsingDisposesWhenBodyThrowsBeforeFirstEffect() =
        let disposed = ref 0
        let resource =
            { new System.IDisposable with
                member _.Dispose() = disposed := !disposed + 1 }
        let program =
            effect {
                use _ = resource
                do raise (System.InvalidOperationException "boom")
                return ()
            }
        Assert.Throws<InvalidOperationException>(fun () ->
            StateEnv(0).Handler.Run(program) |> ignore)
        |> ignore
        Assert.AreEqual(1, !disposed)

    [<TestMethod>]
    member _.UsingAllowsNullResource() =
        let program =
            effect {
                use _ = (null : System.IDisposable)
                return 1
            }
        let result, _ = StateEnv(0).Handler.Run(program)
        Assert.AreEqual(1, result)

    [<TestMethod>]
    member _.TryWithDoesNotCatchFollowingContinuation() =
        let attempts = ref 0
        let program =
            effect {
                try
                    do! State.put 1
                with _ ->
                    do! State.put 2

                attempts := !attempts + 1
                do! (Delay (fun () -> raise (System.InvalidOperationException "boom")) : Program<_, unit>)
                return 3
            }
        Assert.Throws<InvalidOperationException>(fun () ->
            StateEnv(0).Handler.Run(program) |> ignore)
        |> ignore
        Assert.AreEqual(1, !attempts)

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

    [<TestMethod>]
    member _.HandlerEnvironmentBaseClass() =
        let env = StateEnvWithBase(0)
        let program =
            effect {
                do! State.put 9
                return! State.get
            }
        let result, state = env.Handler.Run(program)
        Assert.AreEqual(9, result)
        Assert.AreEqual(9, state)
        Assert.AreSame(env.Handler, env.Handler)
