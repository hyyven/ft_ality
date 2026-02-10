type mode =
    | NoMode
    | Keys
    | Moves

val gnl_grammar : Types.automate -> Types.automate

val parse_line : mode -> string -> Types.automate -> mode * Types.automate