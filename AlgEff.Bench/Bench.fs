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

module Program =

    [<EntryPoint>]
    let main _ =
        BenchmarkRunner.Run<StateBench>() |> ignore
        0
