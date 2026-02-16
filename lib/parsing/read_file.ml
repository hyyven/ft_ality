type mode =
    | NoMode
    | Keys
    | Section of string

let parse_keys (line: string) (automate: Types.automate) : Types.automate =
    let parts = String.split_on_char '=' line in
    match parts with
    | [] | [_] -> automate
    | key_part :: value_parts ->
        let key = String.trim key_part in
        let value = String.trim (String.concat "=" value_parts) in
        if key = "" || value = ""
            then automate
        else
            (* let () = Printf.printf "key_dict: %s -> %s\n" key value in *)
            Modif_automate.add_lexique automate key value

let parse_moves (line: string) (current_mode: mode) (automate: Types.automate) =
    match current_mode with
    | NoMode | Keys -> automate
    | Section name ->
        let parts = String.split_on_char '=' line in
        match parts with
        | [] | [_] -> automate
        | key_part :: value_parts ->
            let key = String.trim key_part in
            let value = String.trim (String.concat "=" value_parts) in
            if key = "" || value = ""
                then automate
            else
                (* 'filter' parcourt la liste return par le split et check si chaque element n'est pas egal a "" *)
                (* 'map' remplace chaque token par sa valeur correspondante dans le lexique *)
                let sequence =
                    String.split_on_char ' ' value
                    |> List.filter (fun s -> s <> "")
                    |> List.map (fun token ->
                        match List.assoc_opt token automate.lexique with
                        | Some v -> v
                        | None ->
                            Printf.printf "error: token '%s' not found in [keys] section\n" token;
                            exit 1
                    )
                in
                let new_move = {
                    Types.nom = key;
                    Types.perso = name;
                    Types.sequence = sequence;
                } in
                Modif_automate.add_move automate new_move

let parse_line (current_mode: mode) (line: string) (automate: Types.automate) : mode * Types.automate =
    let trimmed = String.trim line in
    let len = String.length trimmed in
    let is_section = len >= 2 && trimmed.[0] = '[' && trimmed.[len - 1] = ']' in
    if trimmed = ""
        then (current_mode, automate)
    else if trimmed = "[keys]"
        then (Keys, automate)
    else if is_section
        then (Section (String.sub trimmed 1 (len - 2)), automate)
    else
    (
        match current_mode with
        | Keys -> (current_mode, parse_keys trimmed automate)
        | Section _ -> (current_mode, parse_moves trimmed current_mode automate)
        | NoMode -> (current_mode, automate)
    )

let gnl_grammar (automate: Types.automate) (gmr_file_path: string) : Types.automate =
    try
        In_channel.with_open_text gmr_file_path (fun chan ->
            let rec loop mode automate =
                match In_channel.input_line chan with
                | Some line ->
                    let (next_mode, next_automate) = parse_line mode line automate in
                    loop next_mode next_automate
                | None ->
                    automate
            in
            loop NoMode automate
        )
    with Sys_error msg->
        Printf.printf "Error opening file: %s\n" msg;
        automate