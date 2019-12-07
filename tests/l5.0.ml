(* taint *)

let secret_password () =
    "my_password12345"

let print_multi (a : string) (b : int) (c : string) =
    let _ = print_string a in 
    let _ = print_int b in 
    print_string c

let () = 
    let password = secret_password () in
    let _ = print_string "running program" in
    let _ = print_newline () in
    print_multi password 3 "okay"

