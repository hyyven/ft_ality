type user_event =
	| InputToken of string
	| Reset
	| Quit
	| Ignore

let enable_raw_input_mode () : unit =
    ignore (Sys.command "stty -icanon -echo min 1 time 0")

let disable_raw_input_mode () : unit =
    ignore (Sys.command "stty sane")

let () = at_exit disable_raw_input_mode

let print_key_mappings (automate: Types.automate) : unit =
	print_endline "------------------------------------------";
    print_endline "key mappings:";
	automate.lexique
	|> List.rev
	|> List.iter (fun (key, value) ->
		print_endline (key ^ " -> " ^ value)
	);
	print_endline "------------------------------------------";
	print_endline "controls: esc/ctrl+d quit, backspace reset";
	print_endline "";
	flush stdout

let resolve_symbol (automate: Types.automate) (token: string) : string option =
	match List.assoc_opt token automate.lexique with
	| Some symbol -> Some symbol
	| None -> None

let debug_raw_input (c: char) : unit =
	let code = string_of_int (int_of_char c) in
	Debug.ft_debug ("raw input: " ^ code ^ "\n")

let read_debug_char () : char option =
	try
		let c = input_char stdin in
		debug_raw_input c;
		Some c
	with End_of_file ->
		None

let arrow_token_of_char (c: char) : string option =
	match c with
	| 'A' -> Some "up"
	| 'B' -> Some "down"
	| 'C' -> Some "right"
	| 'D' -> Some "left"
	| _ -> None

let read_arrow_event () : user_event =
	match read_debug_char () with
	| Some '[' ->
		(match read_debug_char () with
		| Some arrow ->
			(match arrow_token_of_char arrow with
			| Some token -> InputToken token
			| None -> Quit)
		| None -> Quit)
	| _ -> Quit

let read_user_event () : user_event =
	match read_debug_char () with
	| None -> Quit
	| Some c ->
		match c with
		| '\004' -> Quit
		| '\027' -> read_arrow_event ()
		| '\127' -> Reset
		| '\n' | '\r' | '\t' | ' ' -> Ignore
		| char -> InputToken (String.make 1 char) (* transforme input char en string et return*)
