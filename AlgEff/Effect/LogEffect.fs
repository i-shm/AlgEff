namespace AlgEff.Effect

/// Logs the given string.
type LogEffect<'next>(str : string, cont : unit -> 'next) =
    inherit Effect<'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        LogEffect(str, cont >> f) :> _

    /// Human-readable name.
    override _.Name = sprintf "Log(%s)" str

    /// String to log.
    member _.String = str

    /// Continuation to next effect.
    member _.Cont = cont

/// Log context requirement.
type LogContext = interface end

module Log =

    /// Logs the given string.
    let write<'ctx when 'ctx :> LogContext> str : Program<'ctx, _> =
        Program.Effect (LogEffect(str, Pure))

    /// Formats and logs a string.
    let writef fmt = Printf.ksprintf write fmt
