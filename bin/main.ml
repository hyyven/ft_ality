let print_automate(automate: Ft_ality.Types.automate) : unit =
    Printf.printf "\n === AUTOMATE === \n";
    Printf.printf "etats : %d etats au total\n" (List.length automate.etats);
    let entrees = String.concat ", " automate.lexique in
    Printf.printf "inputs : %s\n" entrees;
    Printf.printf "etat de depart : %d\n" automate.etat_initial;
    let etats_fin = String.concat ", " 
    (List.map string_of_int automate.etats_finaux) in
    Printf.printf "etats 2 fin : %s\n" etats_fin;
    Printf.printf "Transitions :\n";
    List.iter (fun (src, symbole, dest) ->
        Printf.printf "  (%d, %s) -> %d\n" src symbole dest) automate.transitions;
    
    Printf.printf "\n"


let test_automate () : Ft_ality.Types.automate =
{
    etats = [0; 1; 2];
    lexique = ["[BP]"; "[FP]"];
    etat_initial = 0; 
    etats_finaux = [2];
    transitions = 
    [
    (0, "[BP]", 1);
    (1, "[FP]", 2);
    ];
}

let main () : unit =
    let mon_automate = test_automate () in
    print_automate mon_automate

let () = main ()