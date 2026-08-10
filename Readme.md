# AlgEff - Algebraic Effects for F#

## What are algebraic effects?

Algebraic effects provide a way to define and handle side-effects in  functional programming. This approach has several important benefits:
* Effects are defined in a purely functional way. This eliminates the danger of unexpected side-effects in otherwise pure functional code.
* Implementation of effects (via "handlers") is separate from the effects' definitions. You can handle a given effect type multiple different ways, depending on your needs. (For example, you can use one handler for unit tests, and another for production.)

In summary, you can think of algebraic effects as functional programming's answer to dependency injection in object-oriented programming. They solve a similar problem, but in a more functional way.

## Why use AlgEff?

AlgEff is one of the few algebraic effect systems for F#. It was inspired by a similar F# library called [Eff](https://github.com/palladin/Eff) and by [Scala's ZIO](https://zio.dev/). Reasons to use AlgEff:
* Effects are easy to define.
* Handlers are easy to define.
* Programs that use effects and handlers are easy to write.
* Strong typing reduces the possibility of unhandled effects.

## Writing an effectful program

Let's write a simple effectful program that interacts with the user via a console and then logs the result:

```fsharp
let program =
    effect {
        do! Console.writeln "What is your name?"
        let! name = Console.readln
        do! Console.writelnf "Hello %s" name
        do! Log.writef "Name is %s" name
        return name
    }
```

The type of this value is:

```fsharp
Program<'ctx, string when 'ctx :> LogContext and 'ctx :> ConsoleContext>
```

The first type parameter (`'ctx`) indicates that the program requires handlers for both logging and console effects, and the second one (`string`) indicates that the program returns a string. It's important to understand that this program doesn't actually **do** anything until it's executed. The `program` value itself is purely functional -- no side-effects occurred while creating it.

## 2.0 features

### Loops, exceptions, and resource management

```fsharp
// while / for
let countdown n =
    effect {
        for i in [ n .. -1 .. 1 ] do
            do! Log.writef "%d" i
    }

// while! with an effectful condition
let pollUntilDone isDoneAsync =
    effect {
        while! async { return not (isDoneAsync ()) } do
            do! Log.write "waiting"
    }

// try / finally / use
let withResource r =
    effect {
        use _ = r
        try
            do! State.put 42
        finally
            r.Flush()
    }
```

### Async IO

```fsharp
let fetch = effect {
    let! body = http.getAsync url   // let! binds an Async<'T> directly
    do! Log.writef "Got %d bytes" body.Length
    return body
}
fetch |> handler.RunManyAsync |> Async.RunSynchronously
```

### Error handling

- An unhandled effect raises `UnhandledEffectException`, which includes the effect's name
- Calling `Run` on a multi-result program raises a descriptive `InvalidOperationException` (use `RunMany` instead)
- Exhausting pure console input raises `NoMoreInputException`

### Composition

- `Handler.combine2..5` route effects through an internal type dispatch table (O(1)); nested combination supports any number of handlers
- Subclass effects are not misrouted to handlers declared for their base types
- The `HandlerEnvironment<'env, 'ret, 'st, 'fin>` base class removes the `as this` boilerplate:

```fsharp
type Env() =
    inherit HandlerEnvironment<Env, unit, List<string> * int, List<string> * int>()
    override this.BuildHandler =
        Handler.combine2
            (PureLogHandler(this))
            (PureStateHandler(0, this))
    interface LogContext
    interface StateContext<int>
```

### Semantics

- When a `try` block catches an exception, the handler state is the state at the entry to the `try` block (pure-functional state threading is lost on exception unwinding)
- `Run` and `RunMany` use synchronous fast paths for pure/synchronous handlers, while preserving the same scoped `try/with`, `try/finally`, branch abort, and async fallback semantics as `RunManyAsync`
- An unhandled effect is raised by the run loop as an `UnhandledEffectException` (carrying the effect object), so it can be caught by a program's own `try/with` -- this also guarantees that `try/finally` compensation still runs when an effect is unhandled
- Async computations bound inside multi-shot programs (e.g. `pickAll`) re-execute once per branch
- With two `StateContext` implementations in scope, explicit type annotations are required (e.g. `State.put<int, Env>`)

### Target frameworks and CI

- The NuGet package targets `netstandard2.0`, `net8.0`, and `net10.0`
- `netstandard2.0` keeps broad consumer compatibility
- `net8.0` is the current LTS runtime target
- `net10.0` is the primary development and benchmark target for the 2.0 runtime
- CI builds and tests `net8.0` and `net10.0` on Linux, macOS, and Windows
- BenchmarkDotNet runs are available through a manual GitHub Actions workflow

## Creating a runtime environment

In order to run this program (and potentially cause actual side-effects), we must define an environment that satisfies the program's requirements:

```fsharp
type ProgramEnv<'ret>() as this =
    inherit Environment<'ret>()

    let handler =
        Handler.combine2
            (PureLogHandler(this))
            (ActualConsoleHandler(this))
    
    interface ConsoleContext
    interface LogContext

    member _.Handler = handler
```

The important thing to note here is that our environment contains both a log handler (`PureLogHandler`) and a console handler (`ActualConsoleHandler`). In this case, we've decided to use a log handler that is purely functional (it doesn't perform any I/O) and a console handler that invokes a real command-line console. We could easily have made other choices.

## Running an effectful program

Now that we have both a program and an environment that satisfies its requirements, we can actually run it:
```fsharp
let name, (log, NoState) =
    ProgramEnv().Handler.Run(program)
```

Running a program returns a 2-tuple where the first element is the value returned by the program (`name`) and the second element is the final state of the environment's handlers. Because there are two handlers, the final state is itself a 2-tuple containing the log (`log`) and the console state. The actual console handler is stateless (it performs side-effects directly instead of simulating I/O in memory), so its state value is `NoState`. The resulting console might look like this:

```
What is your name?
Kristin
Hello Kristin
```

And the corresponding log would contain a single entry:

```
Name is Kristin
```

## Defining an effect

To define your own effect, inherit a new type from the base `Effect` type:

```fsharp
/// Logs the given string.
type LogEffect<'next>(str : string, cont : unit -> 'next) =
    inherit Effect<'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        LogEffect(str, cont >> f) :> _

    /// String to log.
    member _.String = str

    /// Continuation to next effect.
    member _.Cont = cont
```

## Handling an effect

Handling effects is also straightforward. The following handles log effects by accumulating strings in a list:

```fsharp
type PureLogHandler<'env, 'ret when 'env :> LogContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, List<string>>()

    /// Start with an empty log.
    override _.Start = []

    /// Adds a string to the log.
    override _.TryStep(log, effect, cont) =
        Handler.tryStep effect (fun (logEff : LogEffect<_>) ->
            let log' = logEff.String :: log
            let next = logEff.Cont()
            cont log' next)

    /// Puts the log in chronological order.
    override _.Finish(log) = List.rev log

    /// Handles LogEffect.
    override _.HandledEffectTypes = [ typeof<LogEffect<Program<'env, 'ret>>> ]
```
