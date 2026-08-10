namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureStateEnv<'state, 'ret>(initial : 'state) as this =
    inherit Environment<'ret>()

    let handler = PureStateHandler(initial, this)
    
    interface StateContext<'state>

    member _.Handler = handler

[<TestClass>]
type StateTest() =

    [<TestMethod>]
    member _.State() =

        let program =
            effect {
                let! x = State.get
                do! State.put (x + 1)
                let! y = State.get
                do! State.put (y + y)
                let! z = State.get
                return z.ToString()
            }

        let result, state =
            PureStateEnv(1).Handler.Run(program)
        Assert.AreEqual<string>("4", result)
        Assert.AreEqual<int>(4, state)

    [<TestMethod>]
    member _.RunManyHandlesLongLinearPrograms() =

        let rec loop n =
            effect {
                if n = 0 then
                    return! State.get<int, PureStateEnv<int, int>>
                else
                    do! State.put<int, PureStateEnv<int, int>> n
                    return! loop (n - 1)
            }

        let result, state =
            PureStateEnv<int, int>(0).Handler.RunMany(loop 100000)
            |> List.exactlyOne

        Assert.AreEqual<int>(1, result)
        Assert.AreEqual<int>(1, state)
