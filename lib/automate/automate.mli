val get_prochain_etat : Types.automate -> Types.etat -> string -> Types.etat option

val get_coups : Types.automate -> Types.etat -> Types.move list option

val construction_automate : Types.grammaire -> Types.automate -> Types.automate

val print_automate : Types.automate -> Types.grammaire -> unit

