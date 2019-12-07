(* no taint  *)

let secret_password () = 
    "my_password1234"

let check_condition () =
    true

let () = 
    let _ = match check_condition () with 
    | true -> print_newline ()
    | false -> print_string "something okay" in
    print_string ("safe") 

