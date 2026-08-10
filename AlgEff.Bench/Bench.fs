module AlgEff.Bench

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open AlgEff.Effect
open AlgEff.Handler

type BenchEnv() as this =
    inherit Environment<int>()
    let handler = PureStateHandler(0, this)
    interface StateContext<int>
    member _.Handler = handler

type CombinedBenchEnv() as this =
    inherit Environment<int>()

    let handler =
        Handler.combine5
            (PureLogHandler(this))
            (PureConsoleHandler([], this))
            (PureStateHandler<int, CombinedBenchEnv, int>(0, this))
            (PureStateHandler<string, CombinedBenchEnv, int>("", this))
            (NonDetHandler.pickTrue this)

    interface LogContext
    interface ConsoleContext
    interface StateContext<int>
    interface StateContext<string>
    interface NonDetContext

    member _.Handler = handler

type NonDetBenchEnv() as this =
    inherit Environment<int>()

    let handler = NonDetHandler.pickAll this

    interface NonDetContext

    member _.Handler = handler

[<MemoryDiagnoser>]
type StateBench() =

    [<Params(1000, 10000)>]
    member val Steps = 0 with get, set

    member this.Program =
        let rec loop n =
            effect {
                if n = 0 then
                    return! State.get
                else
                    do! State.put n
                    return! loop (n - 1)
            }
        loop this.Steps

    [<Benchmark>]
    member this.Run() =
        BenchEnv().Handler.Run(this.Program) |> ignore

[<MemoryDiagnoser>]
type CombinedDispatchBench() =

    [<Params(1000, 10000)>]
    member val Steps = 0 with get, set

    member this.Program =
        let rec loop n =
            effect {
                if n = 0 then
                    return 0
                else
                    do! State.put<string, CombinedBenchEnv> "x"
                    return! loop (n - 1)
            }
        loop this.Steps

    [<Benchmark>]
    member this.Run() =
        CombinedBenchEnv().Handler.Run(this.Program) |> ignore

[<MemoryDiagnoser>]
type NonDetBench() =

    [<Params(8, 12)>]
    member val Depth = 0 with get, set

    member this.Program =
        let rec loop depth total =
            effect {
                if depth = 0 then
                    return total
                else
                    let! n = NonDet.choose 1 2
                    return! loop (depth - 1) (total + n)
            }
        loop this.Depth 0

    [<Benchmark>]
    member this.RunMany() =
        NonDetBenchEnv().Handler.RunMany(this.Program) |> ignore

module Program =

    [<EntryPoint>]
    let main args =
        BenchmarkSwitcher.FromAssembly(typeof<StateBench>.Assembly).Run(args) |> ignore
        0
