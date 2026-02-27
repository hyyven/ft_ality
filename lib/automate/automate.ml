let get_prochain_etat (automate: Types.automate) (etat_actuel: Types.etat) (symbole: string)
    : Types.etat option = 

    (* syntax Lambda -> si args ds la list alr fct ret opt Types.etat*)
    try 
        let (_, _, etat_suivant) = 
            List.find
                (fun (src, sym, _) ->
                    src = etat_actuel && sym = symbole
                )
                automate.Types.transitions
        in
        Some etat_suivant
    with Not_found ->
        None

(*Si etat en arg find dans etats_finaux, on envoie la liste de moves*)
let get_coups (automate: Types.automate) (etat: Types.etat)
    : Types.move list option =
    try
        let moves =
            List.assoc etat automate.Types.etats_finaux
        in
        Some moves
    with Not_found ->
        None



let construction_automate (grammar: Types.grammaire) (automate: Types.automate)
    : Types.automate = 

    let etat_initial = 0 in
    let prochain_etat = ref 1 in

    let table_etats = ref [([], etat_initial)] in
    
    (*tjr pareil, check existance ds list, sauf que si existe pas on cree et insert*)
    let create_get_etat (sequence: string list) : Types.etat =
        try
            List.assoc sequence !table_etats
        with Not_found -> 
            let nouvel_etat = !prochain_etat in
            prochain_etat := !prochain_etat + 1;
            table_etats := (sequence, nouvel_etat) :: !table_etats;
            nouvel_etat
    in 
    (*rec pour chaque move, puis rec pour chaque input de chaque move 
    (Comme une double boucle) avec Pattern matching au lieu de conditions
    Si il reste on creer transi et on rappel avec +1 sur la sequence sinn etat final*)
    let rec construire_pour_move move transitions finals = 
        let rec construire_prefixe prefixe restant trans fins = 
            match restant with
            | [] -> 
                let etat_final = create_get_etat prefixe in
                let fins_maj =
                    try
                        let moves_existants = List.assoc etat_final fins in
                        (etat_final, move :: moves_existants) :: 
                            (List.remove_assoc etat_final fins)
                    with Not_found ->
                        (etat_final, [move]) :: fins
                in
                (trans, fins_maj)
            | symbole :: suite ->
                let etat_courant = create_get_etat prefixe in
                let prochain_prefixe = prefixe @ [symbole] in
                let etat_suivant = create_get_etat prochain_prefixe in

                let nouvelle_transition = 
                    if List.mem (etat_courant, symbole, etat_suivant) trans then
                        trans
                    else
                        (etat_courant, symbole, etat_suivant) :: trans
                in
                construire_prefixe prochain_prefixe suite nouvelle_transition fins
                    
        in

        construire_prefixe [] move.Types.sequence transitions finals
    in

    let (transitions_finales, etats_finaux_finaux) = 
        List.fold_left
            (fun (trans, fins) move ->
                construire_pour_move move trans fins
            )
            ([], [])
            grammar.Types.moves
    in
    let tous_les_etats = List.map snd !table_etats in
    {
        etats = tous_les_etats;
        lexique = automate.Types.lexique;
        etat_initial = etat_initial;
        etats_finaux = etats_finaux_finaux;
        transitions = transitions_finales;
    }


(* FAIT PAR l'IA, on va laisser ta fonction et faire sauter elle c'etait pour tester *)
let print_automate (automate: Types.automate) (grammar: Types.grammaire) : unit =
    Debug.ft_debug "=== AUTOMATE ===\n";
    Debug.ft_debug ("Nombre d'etats : " ^ string_of_int (List.length automate.etats) ^ "\n");
    
    let entrees =
        automate.lexique
        |> List.map (fun (key, value) -> key ^ " -> " ^ value)
        |> String.concat ", "
    in
    Debug.ft_debug ("Lexique : " ^ entrees ^ "\n");
    Debug.ft_debug ("Etat initial : " ^ string_of_int automate.etat_initial ^ "\n");
    
    let etats_fin = 
        automate.etats_finaux
        |> List.map (fun (etat, moves) ->
             let move_names = List.map (fun m -> m.Types.nom) moves |> String.concat ", " in
             string_of_int etat ^ " (" ^ move_names ^ ")")
        |> String.concat ", "
    in
    Debug.ft_debug ("Etats finaux : " ^ etats_fin ^ "\n");
    
    Debug.ft_debug "Transitions :\n";
    List.iter (fun (src, symbole, dest) ->
        Debug.ft_debug ("  (" ^ string_of_int src ^ ", " ^ symbole ^ ") -> " ^ string_of_int dest ^ "\n")
    ) automate.transitions;
    
    Debug.ft_debug "Moves :\n";
    List.iter (fun (move: Types.move) ->
        let seq_str = String.concat " " move.sequence in
        Debug.ft_debug ("  [" ^ move.perso ^ "] " ^ move.nom ^ " = " ^ seq_str ^ "\n")
    ) grammar.moves;
    
    Debug.ft_debug "\n"