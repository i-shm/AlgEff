namespace AlgEff.Test

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type ActualConsoleEnv<'ret>(reader : TextReader, writer : TextWriter) as this =
    inherit Environment<'ret>()

    let handler = ActualConsoleHandler(reader, writer, this)

    interface ConsoleContext

    member _.Handler = handler

type CancellationAwareReader(started : ManualResetEventSlim) =
    inherit TextReader()

    override _.ReadLine() =
        raise (InvalidOperationException("RunManyAsync should use asynchronous console input"))

    override _.ReadLineAsync(cancellationToken : CancellationToken) =
        started.Set() |> ignore
        let completion = TaskCompletionSource<string>(TaskCreationOptions.RunContinuationsAsynchronously)
        cancellationToken.Register(fun () ->
            completion.TrySetCanceled(cancellationToken) |> ignore)
        |> ignore
        ValueTask<string>(completion.Task)

[<TestClass>]
type ActualConsoleTest() =

    [<TestMethod>]
    member _.RunManyAsyncUsesInjectedReaderAndWriter() =
        use reader = new StringReader("Ada\n")
        use writer = new StringWriter()
        let program =
            effect {
                do! Console.writeln "What is your name?"
                let! name = Console.readln
                do! Console.writelnf "Hello %s" name
                return name
            }

        let result, NoState =
            ActualConsoleEnv(reader, writer).Handler.RunManyAsync(program)
            |> Async.RunSynchronously
            |> List.exactlyOne

        Assert.AreEqual("Ada", result)
        Assert.AreEqual("What is your name?\nHello Ada\n", writer.ToString().Replace("\r\n", "\n"))

    [<TestMethod>]
    member _.RunManyAsyncCancelsActualConsoleReadAndRunsCleanup() =
        use started = new ManualResetEventSlim(false)
        use disposed = new ManualResetEventSlim(false)
        use cancelled = new ManualResetEventSlim(false)
        use cancellation = new CancellationTokenSource()
        let reader = new CancellationAwareReader(started)
        use writer = new StringWriter()
        let resource =
            { new IDisposable with
                member _.Dispose() = disposed.Set() |> ignore }
        let program =
            effect {
                use _ = resource
                let! _ = Console.readln
                return ()
            }

        Async.StartWithContinuations(
            ActualConsoleEnv(reader, writer).Handler.RunManyAsync(program),
            ignore,
            ignore,
            (fun _ -> cancelled.Set() |> ignore),
            cancellation.Token)

        Assert.IsTrue(started.Wait 1000)
        cancellation.Cancel()
        Assert.IsTrue(disposed.Wait 1000)
        Assert.IsTrue(cancelled.Wait 1000)
