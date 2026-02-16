module Types = Ft_ality.Types
module Read_file = Ft_ality.Read_file

let print_automate(automate: Types.automate) (grammar: Types.grammaire) : unit =
    Printf.printf "\n === AUTOMATE === \n";
    Printf.printf "etats : %d etats au total\n" (List.length automate.etats);
    let entrees =
        automate.lexique
        |> List.map (fun (key, value) -> key ^ "->" ^ value)
        |> String.concat ", "
    in
    Printf.printf "inputs : %s\n" entrees;
    Printf.printf "etat de depart : %d\n" automate.etat_initial;
    let etats_fin = String.concat ", " 
    (List.map string_of_int automate.etats_finaux) in
    Printf.printf "etats 2 fin : %s\n" etats_fin;
    Printf.printf "Transitions :\n";
    List.iter (fun (src, symbole, dest) ->
        Printf.printf "  (%d, %s) -> %d\n" src symbole dest) automate.transitions;
    
    Printf.printf "moves :\n";
    List.iter
    (
        fun (move: Types.move) ->
        let seq_str = String.concat " " move.sequence in
        Printf.printf "  character: %s, move: %s, sequence: %s\n" move.perso move.nom seq_str
    ) grammar.moves;

    Printf.printf "\n"

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

let main () : unit =
    if Array.length Sys.argv != 2 then
    (
        Printf.printf "usage: ./ft_ality path/to/grammar/file\n";
        exit 1;
    );
    let (mon_automate, ma_grammaire) = test_automate () in
    let (mon_automate, ma_grammaire) = Read_file.gnl_grammar mon_automate ma_grammaire Sys.argv.(1) in
    print_automate mon_automate ma_grammaire

let () = main ()