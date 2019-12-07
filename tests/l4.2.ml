(* taint  *)

let secret_password () = 
    "my_password1234"

let check_condition () =
    true

let () = 
    let _ = if check_condition () 
        then print_string "okay" 
        else print_newline () in
    print_string ( secret_password () ) 


