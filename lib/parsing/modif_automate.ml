let set_lexique_head (automate: Types.automate) (key: string) (value: string) : Types.automate =
    let new_head = (key, value) in
    let new_lexique =
        match automate.lexique with
        | _ :: tail -> new_head :: tail
        | [] -> [new_head]
    in
    { automate with lexique = new_lexique }

let add_lexique (automate: Types.automate) (key: string) (value: string) : Types.automate =
    { automate with lexique = (key, value) :: automate.lexique }

let add_move (automate: Types.automate) (move: Types.move) : Types.automate =
    let current_moves = automate.grammar.moves in
    let new_moves = move :: current_moves in
    { automate with grammar = { Types.moves = new_moves } }
