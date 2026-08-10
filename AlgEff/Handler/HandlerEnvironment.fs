namespace AlgEff.Handler

/// Base type for concrete classes that satisfy an effect type's context
/// requirement. Effects encode this as their 'ctx generic parameter.
[<AbstractClass>]
type Environment<'ret>() = class end

/// State type for handlers that maintain no internal state.
///
/// This keeps no-state handlers explicit without overloading unit in signatures
/// that already use unit as a return value.
[<Struct>]
type NoState = NoState

/// Convenience environment base class: builds and caches the combined handler
/// on first access. BuildHandler is delayed so derived environments may safely
/// reference `this` while wiring handlers together.
[<AbstractClass>]
type HandlerEnvironment<'env, 'ret, 'st, 'fin>() =
    inherit Environment<'ret>()
    let mutable handler : Handler<'env, 'ret, 'st, 'fin> option = None

    /// Builds the combined handler on first access to Handler.
    abstract member BuildHandler : Handler<'env, 'ret, 'st, 'fin>

    /// The combined handler, built once and cached for subsequent accesses.
    member this.Handler =
        match handler with
            | Some h -> h
            | None ->
                let h = this.BuildHandler
                handler <- Some h
                h
