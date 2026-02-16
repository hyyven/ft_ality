type mode =
    | NoMode
    | Keys
    | Section of string

let parse_moves_sequence (value: string) (automate: Types.automate) : string list =
    (* Printf.printf "parsing move: %s" value; *)
    value
    |> String.map (fun c -> if c = '\t' || c = '\r' then ' ' else c)    (* remplace \t et \r par espaces *)
    |> String.split_on_char ' '                                         (* split sur espace *)
    |> List.filter (fun s -> s <> "")                                   (* filtre les elements de la liste qui sont vides*)
    |> List.map (fun token ->                                           (* map chaque token vers sa valeur dans le lexique, print une erreur si l'input existe pas *)
        match List.assoc_opt token automate.lexique with
        | Some v -> v
        | None ->
            Printf.printf "error: token '%s' not found in [keys] section\n" token;
            exit 1
    )

let parse_keys (line: string) (automate: Types.automate) : Types.automate =
    let parts = String.split_on_char '=' line in
    match parts with
    | [] -> Printf.printf "ignoring malformated line: %s\n" line; automate
    | [_] -> Printf.printf "ignoring malformated line: %s\n" line; automate
    | key_part :: value_parts ->
        let key = String.trim key_part in
        let value = String.trim (String.concat "=" value_parts) in
        if key = "" || value = "" then
        (
            Printf.printf "ignoring malformated line (empty key or value): %s\n" line;
            automate
        )
        else
            (* let () = Printf.printf "key_dict: %s -> %s\n" key value in *)
            Modif_automate.add_lexique automate key value

let parse_moves (line: string) (current_mode: mode) (automate: Types.automate) (grammar: Types.grammaire) =
    match current_mode with
    | NoMode | Keys -> grammar
    | Section name ->
        let parts = String.split_on_char '=' line in
        match parts with
        | [] -> Printf.printf "ignoring malformated line: %s\n" line; grammar
        | [_] -> Printf.printf "ignoring malformated line: %s\n" line; grammar
        | key_part :: value_parts ->
            let key = String.trim key_part in
            let value = String.trim (String.concat "=" value_parts) in
            if key = "" || value = "" then
                grammar
            else
                let sequence = parse_moves_sequence value automate in
                (* Printf.printf " -> %s\n" key; *)
                let new_move = {
                    Types.nom = key;
                    Types.perso = name;
                    Types.sequence = sequence;
                    } in
                Modif_automate.add_move grammar new_move

let parse_line (current_mode: mode) (line: string) (automate: Types.automate) (grammar: Types.grammaire) : mode * Types.automate * Types.grammaire =
    let trimmed = String.trim line in
    let len = String.length trimmed in
    let is_section = len >= 2 && trimmed.[0] = '[' && trimmed.[len - 1] = ']' in
    (* Printf.printf "trimmed: %s\n" trimmed; *)
    if trimmed = "" then
        (current_mode, automate, grammar)
    else if is_section then
        if trimmed = "[keys]" then
            match current_mode with
            | NoMode -> (Keys, automate, grammar)
            | _ -> Printf.printf "error: [keys] section must be at the beginning\n"; exit 1
        else
            match current_mode with
            | NoMode -> Printf.printf "error: first section must be [keys]\n"; exit 1
            | _ -> (Section (String.sub trimmed 1 (len - 2)), automate, grammar)
    else
        match current_mode with
        | Keys -> (current_mode, parse_keys trimmed automate, grammar)
        | Section _ -> (current_mode, automate, parse_moves trimmed current_mode automate grammar)
        | NoMode -> (current_mode, automate, grammar)

let gnl_grammar (automate: Types.automate) (grammar: Types.grammaire) (gmr_file_path: string) : Types.automate * Types.grammaire =
    try
        In_channel.with_open_text gmr_file_path (fun chan ->
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
        Printf.printf "Error opening file: %s\n" msg;
        (automate, grammar)