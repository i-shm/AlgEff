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
    let rec bind (f : 'A -> Program<'Ctx, 'B>) (program : Program<'Ctx, 'A>) : Program<'Ctx, 'B> =
        match program with
            | Effect (effect : Effect<'Ctx, 'A>) ->
                effect.Map(bind f) |> Program.Effect
            | Pure x ->
                f x
            | Delay thunk ->
                Delay (fun () -> bind f (thunk ()))
            | Await (node : AwaitNode<'Ctx, 'A>) ->
                Await (ObjectAwaitImpl<'Ctx, 'B>(node.Computation, fun value -> bind f (node.Continuation value)))

/// Internal exception scope contract (intercepted by the run loop, never routed to user handlers).
type ICatchEffect<'ctx> =

    /// The protected computation (exactly the try body), boxed so the run loop can execute it generically.
    abstract member BodyObject : Program<'ctx, obj>

    /// Exception handler (the with clause), boxed like BodyObject.
    abstract member HandlerObject : exn -> Program<'ctx, obj>

    /// Continuation running after the scope resolves.
    abstract member ContinueObject : obj -> obj

/// Internal native resource scope contract (intercepted by the run loop, never routed to user handlers).
type IFinallyEffect<'ctx> =

    /// The scoped computation, boxed so the run loop can execute it generically.
    abstract member BodyObject : Program<'ctx, obj>

    /// Native cleanup (unit -> unit, per the CE protocol for try/finally).
    abstract member CompensationAction : (unit -> unit) with get

    /// Continuation running after the scope resolves.
    abstract member ContinueObject : obj -> obj

/// Exception scope effect.
/// Program.bind maps the continuation only: the body and handler stay inside the scope.
type CatchEffect<'ctx, 'a, 'next>(body : Program<'ctx, 'a>, handler : exn -> Program<'ctx, 'a>, cont : 'a -> 'next) =
    inherit Effect<'next>()

    member _.Body = body
    member _.Handler = handler
    member _.Cont v = cont v

    override _.Map(g : 'next -> 'b) =
        CatchEffect<'ctx, 'a, 'b>(body, handler, fun v -> g (cont v)) :> Effect<'b>

    interface ICatchEffect<'ctx> with
        member _.BodyObject =
            Program.bind (fun value -> Pure (box value)) body
        member _.HandlerObject e =
            Program.bind (fun value -> Pure (box value)) (handler e)
        member _.ContinueObject value =
            box (cont (value :?> 'a))

/// Native resource scope effect.
/// The compensation runs on success, on exception, on branch abort, and on cancellation.
type FinallyEffect<'ctx, 'a, 'next>(body : Program<'ctx, 'a>, compensation : unit -> unit, cont : 'a -> 'next) =
    inherit Effect<'next>()

    member _.Body = body
    member _.Compensation = compensation
    member _.Cont v = cont v

    override _.Map(g : 'next -> 'b) =
        FinallyEffect<'ctx, 'a, 'b>(body, compensation, fun v -> g (cont v)) :> Effect<'b>

    interface IFinallyEffect<'ctx> with
        member _.BodyObject =
            Program.bind (fun value -> Pure (box value)) body
        member _.CompensationAction = compensation
        member _.ContinueObject value =
            box (cont (value :?> 'a))

/// Program builder.
type ProgramBuilder() =
    let (>>=) program f = Program.bind f program
    let returnFrom value =
        match value with
            | Delay thunk -> thunk ()
            | _ -> value

    member this.Bind(program, f) = program >>= f
    member this.Bind(computation : Async<'a>, f : 'a -> Program<'ctx, 'b>) =
        Await (AwaitImpl (computation, f))
    member this.Return(value) = Pure value
    member _.ReturnFrom(value) = returnFrom value
    member _.ReturnFromFinal(value) = returnFrom value
    member this.Zero() = Pure ()
    member this.Combine(program1, program2) = program1 >>= (fun () -> program2)
    member _.Delay(f : unit -> Program<'ctx, 'a>) = Delay f
    member this.While(guard : unit -> bool, body : Program<'ctx, unit>) =
        Delay (fun () ->
            if guard () then body >>= (fun () -> this.While(guard, body))
            else this.Zero ())
    member this.For(sequence : seq<'a>, body : 'a -> Program<'ctx, unit>) =
        Delay (fun () ->
            use enumerator = sequence.GetEnumerator()
            let rec loop () =
                if enumerator.MoveNext() then
                    body enumerator.Current >>= (fun () -> loop ())
                else
                    this.Zero ()
            loop ())
    member this.TryWith(comp : Program<'ctx, 'a>, handler : exn -> Program<'ctx, 'a>) =
        Program.Effect (CatchEffect<'ctx, 'a, Program<'ctx, 'a>>(comp, handler, Pure))
    member this.TryFinally(comp : Program<'ctx, 'a>, compensation : unit -> unit) =
        Program.Effect (FinallyEffect<'ctx, 'a, Program<'ctx, 'a>>(comp, compensation, Pure))
    member this.Using(resource : 'a when 'a :> System.IDisposable, body : 'a -> Program<'ctx, 'b>) =
        Program.Effect (FinallyEffect<'ctx, 'b, Program<'ctx, 'b>>(
            Delay (fun () -> body resource),
            (fun () ->
                if not (Object.ReferenceEquals(box resource, null)) then
                    resource.Dispose ()),
            Pure))

[<AutoOpen>]
module ProgramBuilder =

    /// Program builder.
    let effect = ProgramBuilder()
