(* taint *)

let secret_password _ =
    "my_password123"

let the_password = 
    secret_password 12

let () =
    print_string the_password

