namespace AlgEff.Handler

open AlgEff.Effect

/// https://stackoverflow.com/questions/33464319/implement-a-queue-type-in-f
type Queue<'a> = 'a list * 'a list

module Queue =

    let empty =
        [], []

    let isEmpty = function
        | ([], []) -> true
        | _ -> false

    let enqueue e (fs, bs) =
        e :: fs, bs

    let dequeue = function
        | ([], []) -> failwith "Empty queue"
        | (fs, b :: bs) -> b, (fs, bs)
        | (fs, []) ->
            let bs = List.rev fs
            bs.Head, ([], bs.Tail)

/// Pure concurrency handler.
type PureConcurrencyHandler<'env when 'env :> ConcurrencyContext and 'env :> Environment<unit>>(env : 'env) =
    inherit SimpleHandler<'env, unit, Queue<unit -> Program<'env, unit>>>()

    /// Starts with an empty queue of programs.
    override _.Start = Queue.empty

    /// Manages program control.
    override _.TryStep(queue, effect, cont) =
        match box effect with
            | :? ConcurrencyEffect<'env, Program<'env, unit>> as concurrencyEff ->
                let contUnit =
                    box cont :?> HandlerCont<'env, unit, Queue<unit -> Program<'env, unit>>, 'stx>

                let run queue =
                    let getProgram, queue' = queue |> Queue.dequeue
                    contUnit.Continue queue' <| getProgram ()

                let computation =
                    match concurrencyEff.Case with
                        | Fork eff ->
                            let queue' = queue |> Queue.enqueue eff.Cont
                            contUnit.Continue queue' eff.Program
                        | Yield eff ->
                            queue |> Queue.enqueue eff.Cont |> run
                        | Exit eff ->
                            if queue |> Queue.isEmpty then
                                contUnit.Continue queue <| eff.Cont ()
                            else
                                async {
                                    let! aborted = contUnit.Abort queue
                                    let! next = run queue
                                    return aborted @ next
                                }
                Some(box computation :?> Async<List<HandlerOutcome<'retx, 'stx>>>)
            | _ -> None

    /// Handles Fork, Yield, and Exit effects.
    override _.HandledEffectTypes =
        [ typeof<ForkEffect<'env, Program<'env, unit>>>
          typeof<YieldEffect<'env, Program<'env, unit>>>
          typeof<ExitEffect<'env, Program<'env, unit>>> ]
