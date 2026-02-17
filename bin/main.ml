module Types = Ft_ality.Types
module Read_file = Ft_ality.Read_file
module Debug = Ft_ality.Debug

let print_automate(automate: Types.automate) (grammar: Types.grammaire) : unit =
    Debug.ft_debug ("=== AUTOMATE === \n");
    Debug.ft_debug ("etats : " ^  string_of_int (List.length automate.etats) ^ " etats au total\n");
    let entrees =
        automate.lexique
        |> List.map (fun (key, value) -> key ^ "->" ^ value)
        |> String.concat ", "
    in
    Debug.ft_debug ("inputs : " ^ entrees ^ "\n");
    Debug.ft_debug ("etat de depart : " ^ string_of_int automate.etat_initial ^ "\n");
    let etats_fin = String.concat ", " 
    (List.map string_of_int automate.etats_finaux) in
    Debug.ft_debug ("etats 2 fin : " ^ etats_fin ^ "\n");
    Debug.ft_debug ("Transitions :\n");
    List.iter (fun (src, symbole, dest) ->
        Debug.ft_debug ("  (" ^ string_of_int src ^ ", " ^ symbole ^ ") -> " ^ string_of_int dest ^ "\n")
    ) automate.transitions;
    
    Debug.ft_debug ("moves :\n");
    List.iter
    (
        fun (move: Types.move) ->
        let seq_str = String.concat " " move.sequence in
        Debug.ft_debug ("  character: " ^ move.perso ^ ", move: " ^ move.nom ^ ", sequence: " ^ seq_str ^ "\n")
    ) grammar.moves;

    Debug.ft_debug ("\n")

let test_automate () : Types.automate * Types.grammaire =
    let grammar = { Types.moves = [] } in 
    let automate = {
        Types.etats = [0; 1; 2];
        Types.lexique = [];
        Types.etat_initial = 0;
        Types.etats_finaux = [2];
        Types.transitions = 
        [
            (0, "[BP]", 1);
            (1, "[FP]", 2);
        ];
    } in
    (automate, grammar)

let usage () =
    Printf.printf "usage: ./ft_ality path/to/grammar/file [--debug]\n";
    exit 1

let parse_args () : string * bool =
    match Array.to_list Sys.argv with
    | _ :: path :: "--debug" :: [] -> (path, true)
    | _ :: path :: [] -> (path, false)
    | _ -> usage ()

let main () : unit =
    let (grammar_file, debug_mode) = parse_args () in
    if debug_mode then
    (
        Printf.printf "debug mode enabled\n";
        Debug.set_debug_mode true
    );
    let (mon_automate, ma_grammaire) = test_automate () in
    let (mon_automate, ma_grammaire) = Read_file.gnl_grammar mon_automate ma_grammaire grammar_file in
    print_automate mon_automate ma_grammaire

let () = main ()