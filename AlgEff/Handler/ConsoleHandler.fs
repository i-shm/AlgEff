namespace AlgEff.Handler

open System
open AlgEff.Effect

/// Pure functional model of a console.
type PureConsole =
    {
        /// Input to be read by the console (as if they were
        /// typed by the user).
        Input : List<string>

        /// Output written to the console so far.
        Output : List<string>
    }

module PureConsole =

    /// Creates a console.
    let create input output =
        {
            Input = input
            Output = output
        }

/// Pure console handler.
type PureConsoleHandler<'env, 'ret when 'env :> ConsoleContext and 'env :> Environment<'ret>>(input, env : 'env) =
    inherit SimpleHandler<'env, 'ret, PureConsole>()

    /// Console has pending input, but no output yet.
    override _.Start = PureConsole.create input []

    /// Writes to or reads from the console.
    override _.TryStep(state, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    cont.Continue state' next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                PureConsole.create tail output
                            let next = eff.Cont(head)
                            cont.Continue state' next
                        | _ -> raise NoMoreInputException)

    override _.TryStepSync(state, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    cont.Continue state' next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                PureConsole.create tail output
                            let next = eff.Cont(head)
                            cont.Continue state' next
                        | _ -> raise NoMoreInputException)

    override _.TryStepManySync(state, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    cont.ContinueTail state' next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                PureConsole.create tail output
                            let next = eff.Cont(head)
                            cont.ContinueTail state' next
                        | _ -> raise NoMoreInputException)

    override _.TryStepOneSync(state, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    let state' =
                        { state with Output = eff.String :: state.Output }
                    let next = eff.Cont()
                    cont.Continue state' next
                | ReadLine eff ->
                    match state.Input with
                        | head :: tail ->
                            let state' =
                                let output = head :: state.Output
                                PureConsole.create tail output
                            let next = eff.Cont(head)
                            cont.Continue state' next
                        | _ -> raise NoMoreInputException)

    /// Puts console output in chronological order.
    override _.Finish(state) =
        { state with Output = state.Output |> List.rev }

    /// Handles WriteLine and ReadLine effects.
    override _.HandledEffectTypes =
        [ typeof<WriteLineEffect<Program<'env, 'ret>>>
          typeof<ReadLineEffect<Program<'env, 'ret>>> ]

/// Actual console handler.
type ActualConsoleHandler<'env, 'ret when 'env :> ConsoleContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, NoState>()

    /// No internal state to maintain.
    override _.Start = NoState

    /// Writes to or reads from the console.
    override _.TryStep(NoState, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont.Continue NoState next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont.Continue NoState next)

    override _.TryStepSync(NoState, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont.Continue NoState next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont.Continue NoState next)

    override _.TryStepManySync(NoState, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont.ContinueTail NoState next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont.ContinueTail NoState next)

    override _.TryStepOneSync(NoState, effect, cont) =
        Handler.tryStep effect (fun (consoleEff : ConsoleEffect<_>) ->
            match consoleEff.Case with
                | WriteLine eff ->
                    System.Console.WriteLine(eff.String)
                    let next = eff.Cont()
                    cont.Continue NoState next
                | ReadLine eff ->
                    let str = System.Console.ReadLine()
                    let next = eff.Cont(str)
                    cont.Continue NoState next)

    /// Handles WriteLine and ReadLine effects.
    override _.HandledEffectTypes =
        [ typeof<WriteLineEffect<Program<'env, 'ret>>>
          typeof<ReadLineEffect<Program<'env, 'ret>>> ]
