namespace AlgEff.Handler

open System
open AlgEff.Effect

/// 未处理效应异常。
type UnhandledEffectException(effect : obj) =
    inherit System.Exception(sprintf "Unhandled effect %O" effect)
    member _.Effect = effect

/// 纯控制台输入耗尽。
exception NoMoreInputException

/// 处理剩余程序的续体（异步）。
/// 'st:  State type maintained by the handler.
/// 'stx: State type answered by the continuation, which may be
///       different from the state managed by the handler (e.g. the
///       combined state of multiple handlers).
type HandlerCont<'ctx, 'ret, 'st, 'stx> =
    'st -> Program<'ctx, 'ret> -> Async<List<'ret * 'stx>>

/// Effect handler base class.
/// 'ctx: Context type requirement satisfied by this handler.
/// 'ret: Return type of program handled by this handler.
/// 'st:  Internal state type maintained by this handler.
/// 'fin: Final state type produced by this handler.
[<AbstractClass>]
type Handler<'ctx, 'ret, 'st, 'fin>() =

    /// Handler's initial state.
    abstract member Start : 'st

    /// Attempts to handle a single effect in a program.
    /// Returns None if the effect is not handled by this handler.
    /// 'cont' continues the program with the handler's new state.
    abstract member TryStep<'stx> :
        'st                                        // state before handling current effect
            * Effect<'ctx, 'ret>                   // effect to be handled
            * HandlerCont<'ctx, 'ret, 'st, 'stx>   // continuation that will handle the remainder of the program
            -> Option<Async<List<'ret * 'stx>>>    // "Some" indicates the effect was handled

    /// Transforms the handler's final state.
    abstract member Finish : 'st -> 'fin

    /// Effect 类型注册表（分派表用，open generic definition 或具体类型）。
    abstract member HandledEffectTypes : Type list

    /// Runs the given program asynchronously, producing a list of results.
    member this.RunManyAsync(program) : Async<List<'ret * 'fin>> =

        /// Runs a single step in the program.
        let rec loop (state : 'st) (program : Program<'ctx, 'ret>) : Async<List<'ret * 'st>> =
            async {
                match program with
                    | Pure ret -> return [ ret, state ]
                    | Effect effect ->
                        match this.TryStep(state, effect, loop) with
                            | Some computation -> return! computation
                            | None -> return raise (UnhandledEffectException(effect))
                    | Delay f -> return! loop state (f ())
                    | Await node ->
                        let! value = node.Computation
                        return! loop state (node.Continuation value)
                    | Catch (comp, handler) ->
                        try return! loop state comp
                        with e -> return! loop state (handler e)
            }

        async {
            let! results = loop this.Start program
            return
                results
                |> List.map (fun (ret, state) ->
                    ret, this.Finish(state))
        }

    /// Runs the given program, producing a list of results.
    member this.RunMany(program) : List<'ret * 'fin> =
        this.RunManyAsync(program) |> Async.RunSynchronously

    /// Runs the given program, producing a single result.
    member this.Run(program) : 'ret * 'fin =
        match this.RunMany(program) with
            | [ pair ] -> pair
            | [] -> raise (InvalidOperationException("Program produced no results"))
            | results ->
                raise (InvalidOperationException(
                    sprintf "Program produced %d results; use RunMany for multi-shot programs"
                        results.Length))

/// Handler whose final state type is the same as its internal state type.
[<AbstractClass>]
type SimpleHandler<'ctx, 'ret, 'st>() =
    inherit Handler<'ctx, 'ret, 'st, 'st>()

    /// No-op final transformation.
    default _.Finish(state) = state

/// 无任何 handler 的环境处理器（运行任何程序都会得到未处理效应）。
type NoopHandler<'ctx, 'ret, 'st, 'fin>(start : 'st, finish : 'st -> 'fin) =
    inherit Handler<'ctx, 'ret, 'st, 'fin>()
    override _.Start = start
    override _.TryStep(state, effect, _) = None
    override _.Finish(state) = finish state
    override _.HandledEffectTypes = []

/// Combines two effect handlers using the given finish.
type private CombinedHandler<'ctx, 'ret, 'st1, 'fin1, 'st2, 'fin2, 'fin>
    (handler1 : Handler<'ctx, 'ret, 'st1, 'fin1>,
    handler2 : Handler<'ctx, 'ret, 'st2, 'fin2>,
    finish : ('fin1 * 'fin2) -> 'fin) =
    inherit Handler<'ctx, 'ret, 'st1 * 'st2, 'fin>()

    /// Combined initial state.
    override _.Start = handler1.Start, handler2.Start

    /// Attempts to handle a single effect by routing it first to one handler,
    /// and then to the other.
    override _.TryStep((state1, state2), effect, cont) =
        handler1.TryStep(state1, effect, fun state1' program ->
            cont (state1', state2) program)
            |> Option.orElseWith (fun () ->
                handler2.TryStep(state2, effect, fun state2' program ->
                    cont (state1, state2') program))

    /// Combines the given handlers' final states.
    override _.Finish((state1, state2)) =
        finish (handler1.Finish(state1), handler2.Finish(state2))

    override _.HandledEffectTypes =
        handler1.HandledEffectTypes @ handler2.HandledEffectTypes

module Handler =

    /// 无 handler 的处理器（用于测试未处理效应路径）。
    let noopHandler<'ctx, 'ret> : Handler<'ctx, 'ret, unit, unit> =
        NoopHandler((), id) :> _

    /// Adapts a step function for use in an effect handler.
    let tryStep<'eff, 'next, 'ret when 'eff :> Effect<'next>>
        (effect : Effect<'next>)
        (step : 'eff -> 'ret) =
            match effect with
                | :? 'eff as eff -> step eff |> Some
                | _ -> None

    /// Combines two handlers using the given finish.
    let private combine handler1 handler2 finish =
        CombinedHandler(handler1, handler2, finish) :> Handler<_, _, _, _>

    /// Combines two handlers.
    let combine2 handler1 handler2 =
        combine handler1 handler2 id

    /// Combines three handlers.
    let combine3 handler1 handler2 handler3 =
        combine
            (combine2 handler1 handler2)
            handler3
            (fun ((s1, s2), s3) -> s1, s2, s3)

    /// Combines four handlers.
    let combine4 handler1 handler2 handler3 handler4 =
        combine
            (combine3 handler1 handler2 handler3)
            handler4
            (fun ((s1, s2, s3), s4) -> s1, s2, s3, s4)

    /// Combines five handlers.
    let combine5 handler1 handler2 handler3 handler4 handler5 =
        combine
            (combine4 handler1 handler2 handler3 handler4)
            handler5
            (fun ((s1, s2, s3, s4), s5) -> s1, s2, s3, s4, s5)

/// Base type for concrete classes that satisfy an effect type's context
/// requirement.
[<AbstractClass>]
type Environment<'ret>() = class end

/// Unit type replacement.
/// https://stackoverflow.com/questions/47909938/passing-unit-as-type-parameter-to-generic-class-in-f
type Unit = Unit
