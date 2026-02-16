type mode =
    | NoMode
    | Keys
    | Section of string

val gnl_grammar : Types.automate -> string -> Types.automate

val parse_line : mode -> string -> Types.automate -> mode * Types.automate