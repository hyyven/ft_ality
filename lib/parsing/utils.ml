let ft_trim_split (s: string) (c: char) : string list =
    s
    |> String.map (fun ch -> if ch = '\t' || ch = '\r' then c else ch)    (* remplace \t et \r par 'c' *)
    |> String.split_on_char c                                             (* split sur 'c' *)
    |> List.filter (fun s -> s <> "")                                     (* filtre les elements de la liste qui sont vides*)

let ft_check_keys_duplicate (key: string) (value: string) (automate: Types.automate) : unit =
    if List.mem_assoc key automate.lexique then
    (
        Debug.ft_error ("duplicate key '" ^ key ^ "' in [keys] section");
    )
    else if List.exists (fun (_, v) -> v = value) automate.lexique then
    (
        Debug.ft_error ("duplicate value '" ^ value ^ "' in [keys] section");
    )

let ft_check_move_duplicate (name: string) (sequence: string list) (perso: string) (grammar: Types.grammaire) : unit =
    if List.exists (fun (m: Types.move) -> m.perso = perso && m.nom = name) grammar.moves then
    (
        Debug.ft_error ("duplicate move name '" ^ name ^ "' for character '" ^ perso ^ "'");
    )
    else if List.exists (fun (m: Types.move) -> m.perso = perso && m.sequence = sequence) grammar.moves then
    (
        Debug.ft_error ("duplicate move sequence '" ^ name ^ "' for character '" ^ perso ^ "'");
    )

let split_on_first_char (s: string) : (string * string) option =
    match String.index_opt s '=' with
    | Some idx ->
        let key = String.trim (String.sub s 0 idx) in   (* extract la partie entre l'index 0 et idx exclu (l'index du premier '=') *)
        let value = String.trim (String.sub s (idx + 1) (String.length s - idx - 1)) in (* s - idx - 1 c'est la taille a extraire *)
        Some (key, value)
    | None -> None