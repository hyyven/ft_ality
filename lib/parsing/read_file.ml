let parse_line (line: string) =
    Printf.printf "%s\n" line

let gnl_grammar () =
    try
        let chan = open_in "grammar.txt" in
        let rec loop () =
            try
                parse_line (input_line chan);
                loop ()
            with End_of_file ->
                close_in chan
        in
        loop ()
    with Sys_error msg->
        Printf.printf "Error opening file: %s\n" msg