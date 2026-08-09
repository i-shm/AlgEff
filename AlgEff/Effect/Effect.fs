namespace AlgEff.Effect

open System

/// 一个 effectful 操作（自由单子链中的一个节点）。
[<AbstractClass>]
type Effect<'next>() =

    /// Maps a function over this effect.
    abstract member Map : ('next -> 'b) -> Effect<'b>

    /// Effect 的可读名称（错误信息用）。
    abstract member Name : string
    default this.Name = this.GetType().Name

    override this.ToString() = this.Name

/// 异步挂起点（存在类型包装：F# 抽象成员不能带自有类型参数）。
type AwaitNode<'ctx, 'ret>(computation : Async<obj>, continuation : obj -> Program<'ctx, 'ret>) =

    /// 被挂起的异步计算（已装箱）。
    member _.Computation = computation

    /// 异步结果返回后继续执行的续体。
    member _.Continuation = continuation

/// 一个要求特定 context（'ctx）并返回特定结果（'ret）的效应程序（自由单子）。
and Program<'ctx, 'ret> =

    /// 一步效应。
    | Effect of Effect<'ctx, 'ret>

    /// 纯值（程序终点）。
    | Pure of 'ret

    /// 惰性节点（while/for/try 的求值基础）。
    | Delay of (unit -> Program<'ctx, 'ret>)

    /// 异步挂起点。
    | Await of AwaitNode<'ctx, 'ret>

    /// 异常捕获（try/with 与 try/finally 的基础）。
    | Catch of Program<'ctx, 'ret> * (exn -> Program<'ctx, 'ret>)

/// 一个程序中的一步效应。
and Effect<'ctx, 'ret> = Effect<Program<'ctx, 'ret>>

/// obj 版 Await 节点（供 Program.bind 重组用）。
type ObjectAwaitImpl<'ctx, 'ret>(computation : Async<obj>, continuation : obj -> Program<'ctx, 'ret>) =
    inherit AwaitNode<'ctx, 'ret>(computation, continuation)

/// 类型安全的 Await 节点构造器。
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

/// Program builder（CE 标准成员在 Task 4 补齐）。
type ProgramBuilder() =
    let (>>=) program f = Program.bind f program
    member _.Bind(program, f) = program >>= f
    member _.Return(value) = Pure value
    member _.ReturnFrom(value) = value
    member _.Zero() = Pure ()
    member _.Combine(program1, program2) = program1 >>= (fun () -> program2)
    member _.Delay(f) = Delay f

[<AutoOpen>]
module ProgramBuilder =

    /// Program builder.
    let effect = ProgramBuilder()
