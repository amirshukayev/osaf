(* no taint *)

let secret_password _ = 
    "my_password1234"

let safe_func () =
    "safe"

let unsafe_val =
    secret_password ()

let () =
    let password = safe_func () in
    let _ = print_int 123 in 
    print_string (password)

