type mode =
    | NoMode
    | Keys
    | Section of string

val gnl_grammar : Types.automate -> Types.grammaire -> string -> Types.automate * Types.grammaire

val parse_line : mode -> string -> Types.automate -> Types.grammaire -> mode * Types.automate * Types.grammaire