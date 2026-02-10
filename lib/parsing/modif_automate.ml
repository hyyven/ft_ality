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
