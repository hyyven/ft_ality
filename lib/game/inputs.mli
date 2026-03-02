type user_event =
	| InputToken of string
	| Reset
	| Quit
	| Ignore

val enable_raw_input_mode : unit -> unit

val disable_raw_input_mode : unit -> unit

val print_key_mappings : Types.automate -> unit

val resolve_symbol : Types.automate -> string -> string option

val read_user_event : unit -> user_event