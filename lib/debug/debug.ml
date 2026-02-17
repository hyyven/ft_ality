let debug_mode = ref false

let set_debug_mode (mode: bool) =
    debug_mode := mode

let is_debug () : bool =
    !debug_mode

let ft_error (err: string) =
    Printf.printf "[ERROR] %s\n" err;
    exit 1

let ft_debug (str: string) : unit =
    if is_debug () then
        Printf.printf "[DEBUG] %s" str