(* taint *)

let secret_password _ = 
    "my_password1234"

let () =
    let password = secret_password () in
    let _ = print_int 123 in 
    print_string (password)

