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

let print_separator () : unit =
    Printf.printf "------------------------------------------\n"

let has_outgoing (automate: Types.automate) (etat: Types.etat) : bool =
    List.exists (fun (src, _, _) -> src = etat) automate.transitions

let run (automate: Types.automate) : unit =
    Inputs.print_key_mappings automate;
    Inputs.enable_raw_input_mode ();
    let rec loop (etat_courant: Types.etat) (current_combo: string list) =
        match Inputs.read_user_event () with
        | Inputs.Ignore ->
            loop etat_courant current_combo
        | Inputs.Reset ->
            Debug.ft_debug "state reset to 0\n";
            print_separator ();
            flush stdout;
            loop automate.etat_initial []
        | Inputs.Quit ->
            Inputs.disable_raw_input_mode ();
            Printf.printf "\nend\n";
            flush stdout
        | Inputs.InputToken token ->
            Printf.printf "\n";
            (match Inputs.resolve_symbol automate token with
            | None ->
                Debug.ft_debug ("unknown key '" ^ token ^ "'\n");
                print_separator ();
                flush stdout;
                loop automate.etat_initial []
            | Some sym ->
                let transition =
                    match Automate.get_prochain_etat automate etat_courant sym with
                    | Some state -> `Continue (state, current_combo @ [sym])
                    | None ->
                        if etat_courant = automate.etat_initial then
                            `Unknown
                        else
                            `Broken (current_combo @ [sym])
                in
                let proceed next_state new_combo =
                    Printf.printf "%s\n" (String.concat " | " new_combo);
                    (match Automate.get_coups automate next_state with
                    | Some moves -> 
                        List.iter (fun (move: Types.move) ->
                            Debug.ft_debug
                                ("Found end state for \"" ^ move.nom
                                ^ " (" ^ move.perso ^ ")\" at: "
                                ^ string_of_int next_state ^ "\n")
                        ) moves;
                        print_moves moves
                    | None -> ());
                    Debug.ft_debug ("State " ^ string_of_int etat_courant
                                    ^ ", \"" ^ sym ^ "\" -> State "
                                    ^ string_of_int next_state ^ "\n");
                    if not (has_outgoing automate next_state) then
                    begin
                        print_separator ();
                        flush stdout;
                        loop automate.etat_initial []
                    end
                    else
                    begin
                        flush stdout;
                        loop next_state new_combo
                    end
                in
                (match transition with
                | `Unknown ->
                    Printf.printf "%s\n" sym;
                    print_separator ();
                    flush stdout;
                    loop automate.etat_initial []
                | `Broken broken_combo ->
                    Printf.printf "%s\n" (String.concat " | " broken_combo);
                    print_separator ();
                    flush stdout;
                    loop automate.etat_initial []
                | `Continue (next_state, new_combo) ->
                    proceed next_state new_combo))
    in
    loop automate.etat_initial []
