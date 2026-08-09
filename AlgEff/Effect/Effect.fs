namespace AlgEff.Effect

open System

/// An effectful operation (one node in a free monad chain).
[<AbstractClass>]
type Effect<'next>() =

    /// Maps a function over this effect.
    abstract member Map : ('next -> 'b) -> Effect<'b>

    /// Human-readable name for this effect (used in error messages).
    abstract member Name : string
    default this.Name = this.GetType().Name

    override this.ToString() = this.Name

/// Async suspension point (existential type wrapper: F# abstract members cannot have their own type parameters).
type AwaitNode<'ctx, 'ret>(computation : Async<obj>, continuation : obj -> Program<'ctx, 'ret>) =

    /// The suspended async computation (boxed).
    member _.Computation = computation

    /// Continuation to run after the async result returns.
    member _.Continuation = continuation

/// An effectful program (free monad) requiring a specific context ('ctx) and returning a specific result ('ret).
and Program<'ctx, 'ret> =

    /// A single-step effect.
    | Effect of Effect<'ctx, 'ret>

    /// A pure value (terminal point of the program).
    | Pure of 'ret

    /// Lazy node (the evaluation basis for while/for/try).
    | Delay of (unit -> Program<'ctx, 'ret>)

    /// Async suspension point.
    | Await of AwaitNode<'ctx, 'ret>

    /// Exception catch (the basis for try/with and try/finally).
    | Catch of Program<'ctx, 'ret> * (exn -> Program<'ctx, 'ret>)

/// A single-step effect within a program.
and Effect<'ctx, 'ret> = Effect<Program<'ctx, 'ret>>

/// obj-typed Await node (for restructuring within Program.bind).
type ObjectAwaitImpl<'ctx, 'ret>(computation : Async<obj>, continuation : obj -> Program<'ctx, 'ret>) =
    inherit AwaitNode<'ctx, 'ret>(computation, continuation)

/// Type-safe Await node constructor.
type AwaitImpl<'a, 'ctx, 'ret>(computation : Async<'a>, continuation : 'a -> Program<'ctx, 'ret>) =
    inherit AwaitNode<'ctx, 'ret>(
        async {
            let! value = computation
            return box value
        },
        fun value -> continuation (value :?> 'a))

module Program =

    /// Binds two programs together in the same context.
    let rec bind (f : _ -> Program<'ctx, _>) (program : Program<'ctx, _>) =
        match program with
            | Effect effect -> effect.Map(bind f) |> Program.Effect
            | Pure x -> f x
            | Delay thunk -> Delay (fun () -> bind f (thunk ()))
            | Await node ->
                Await (ObjectAwaitImpl<'ctx, _>(node.Computation, fun value -> bind f (node.Continuation value)))
            | Catch (comp, handler) -> Catch (bind f comp, fun e -> bind f (handler e))

/// Program builder.
type ProgramBuilder() =
    let (>>=) program f = Program.bind f program
    member this.Bind(program, f) = program >>= f
    member this.Bind(computation : Async<'a>, f : 'a -> Program<'ctx, 'b>) =
        Await (AwaitImpl (computation, f))
    member this.Return(value) = Pure value
    member _.ReturnFrom(value) = value
    member this.Zero() = Pure ()
    member this.Combine(program1, program2) = program1 >>= (fun () -> program2)
    member _.Delay(f : unit -> Program<'ctx, 'a>) = Delay f
    member this.While(guard : unit -> bool, body : Program<'ctx, unit>) =
        Delay (fun () ->
            if guard () then body >>= (fun () -> this.While(guard, body))
            else this.Zero ())
    member this.For(sequence : seq<'a>, body : 'a -> Program<'ctx, unit>) =
        Delay (fun () ->
            (this.Zero (), sequence) ||> Seq.fold (fun acc item -> acc >>= (fun () -> body item)))
    member this.TryWith(comp : Program<'ctx, 'a>, handler : exn -> Program<'ctx, 'a>) =
        Catch (comp, handler)
    member this.TryFinally(comp : Program<'ctx, 'a>, compensation : Program<'ctx, unit>) =
        let compensationRan = ref false
        let runCompensation () =
            if compensationRan.Value then
                this.Zero ()
            else
                compensationRan.Value <- true
                compensation
        let tryFinallyProgram =
            let withCompensation program =
                program >>= (fun v -> runCompensation () >>= (fun () -> this.Return v))
            Catch (withCompensation comp, fun e -> runCompensation () >>= (fun () -> raise e))
        Delay (fun () ->
            compensationRan.Value <- false
            tryFinallyProgram)
    member this.Using(resource : 'a when 'a :> System.IDisposable, body : 'a -> Program<'ctx, 'b>) =
        let dispose = Delay (fun () -> resource.Dispose (); this.Zero ())
        this.TryFinally (body resource, dispose)

[<AutoOpen>]
module ProgramBuilder =

    /// Program builder.
    let effect = ProgramBuilder()
