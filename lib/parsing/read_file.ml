type mode =
    | NoMode
    | Keys
    | Section of string

let parse_moves_sequence (value: string) (automate: Types.automate) : string list =
    (* Debug.ft_debug ("parsing move: " ^ value ^ "\n"); *)
    Utils.ft_trim_split value ' '
    |> List.map (fun token ->   (* map chaque token vers sa valeur dans le lexique, return une erreur si l'input existe pas *)
        match List.assoc_opt token automate.lexique with
        | Some v -> v
        | None -> Debug.ft_error ("token '" ^ token ^ "' not found in [keys] section");
    )

let parse_keys (line: string) (automate: Types.automate) : Types.automate =
    match Utils.split_on_first_char line with
    | None -> Debug.ft_debug ("ignoring malformated line: " ^ line ^ "\n"); automate
    | Some (key, value) ->
        (* Debug.ft_debug ("value: " ^ value ^ "\n"); *)
        if key = "" || value = "" then
        (
            Debug.ft_debug ("ignoring malformated line (empty key or value): " ^ line ^ "\n");
            automate
        )
        else
        (
            Utils.ft_check_keys_duplicate key value automate;
            Modif_automate.add_lexique automate key value
        )

let parse_moves (line: string) (current_mode: mode) (automate: Types.automate) (grammar: Types.grammaire) =
    match current_mode with
    | NoMode | Keys -> grammar
    | Section name ->
        match Utils.split_on_first_char line with
        | None -> Debug.ft_debug ("ignoring malformated line: " ^ line ^ "\n"); grammar
        | Some (key, value) ->
            if key = "" || value = "" then
            (
                Debug.ft_debug ("ignoring malformated line: " ^ line ^ "\n");
                grammar
            )
            else
            (
                let sequence = parse_moves_sequence value automate in
                (* Debug.ft_debug (" -> " ^ key ^ "\n"); *)
                Utils.ft_check_move_duplicate key sequence name grammar;
                let new_move = {
                    Types.nom = key;
                    Types.perso = name;
                    Types.sequence = sequence;
                } in
                Modif_automate.add_move grammar new_move
            )

let parse_line (current_mode: mode) (line: string) (automate: Types.automate) (grammar: Types.grammaire) : mode * Types.automate * Types.grammaire =
    let trimmed = String.trim line in
    let len = String.length trimmed in
    let is_section = len >= 2 && trimmed.[0] = '[' && trimmed.[len - 1] = ']' in
    (* Debug.ft_debug ("trimmed: " ^ trimmed ^ "\n"); *)
    if trimmed = "" then
        (current_mode, automate, grammar)
    else if is_section then
        if trimmed = "[keys]" then
            match current_mode with
            | NoMode -> (Keys, automate, grammar)
            | _ -> Debug.ft_error "[keys] section must be at the beginning"
        else
            match current_mode with
            | NoMode -> Debug.ft_error "first section must be [keys]"
            | _ -> (Section (String.sub trimmed 1 (len - 2)), automate, grammar)
    else
        match current_mode with
        | Keys -> (current_mode, parse_keys trimmed automate, grammar)
        | Section _ -> (current_mode, automate, parse_moves trimmed current_mode automate grammar)
        | NoMode -> (current_mode, automate, grammar)

let gnl_grammar (automate: Types.automate) (grammar: Types.grammaire) (gmr_file_path: string) : Types.automate * Types.grammaire =
    try
        In_channel.with_open_text gmr_file_path (fun chan ->        (*fonction interdite ?*)
            let rec loop mode automate grammar =
                match In_channel.input_line chan with
                | Some line ->
                    let (next_mode, next_automate, next_grammar) = parse_line mode line automate grammar in
                    loop next_mode next_automate next_grammar
                | None ->
                    (automate, grammar)
            in
            loop NoMode automate grammar
        )
    with Sys_error msg->
        Debug.ft_error ("opening file: " ^ msg)