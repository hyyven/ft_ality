let print_moves (moves: Types.move list) : unit =
	List.iter (fun (move: Types.move) ->
		Printf.printf "%s (%s)\n" move.nom move.perso
	) moves

let apply_symbol (automate: Types.automate) (etat_courant: Types.etat) (symbol: string) : Types.etat =
	let transition_from (etat_src: Types.etat) : Types.etat option =
		Automate.get_prochain_etat automate etat_src symbol
	in
	let next_state =
		match transition_from etat_courant with
		| Some state -> Some state
		| None -> transition_from automate.etat_initial
	in
	match next_state with
	| Some etat_suivant ->
		(match Automate.get_coups automate etat_suivant with
		| Some moves -> print_moves moves
		| None -> ());
		etat_suivant
	| None ->
		Printf.printf "unknown sequence for input '%s'\n" symbol;
		automate.etat_initial

let process_token (automate: Types.automate) (etat_courant: Types.etat) (token: string) : Types.etat =
	match Inputs.resolve_symbol automate token with
	| Some symbol ->
		apply_symbol automate etat_courant symbol
	| None ->
		Debug.ft_debug ("unknown key '" ^ token ^ "'\n");
		automate.etat_initial

let run (automate: Types.automate) : unit =
	Inputs.print_key_mappings automate;
	Inputs.enable_raw_input_mode ();
	let rec loop (etat_courant: Types.etat) =
		match Inputs.read_user_event () with
		| Inputs.Ignore ->
			loop etat_courant
		| Inputs.Reset ->
			Debug.ft_debug "state reset to 0\n";
			flush stdout;
			loop automate.etat_initial
		| Inputs.Quit ->
			Inputs.disable_raw_input_mode ();
			Printf.printf "\nend\n";
			flush stdout
		| Inputs.InputToken token ->
			let next_state = process_token automate etat_courant token in
			Debug.ft_debug ("state: " ^ string_of_int next_state ^ "\n");
      Printf.printf "------------------------------------------\n";
			flush stdout;
			loop next_state
	in
	loop automate.etat_initial
