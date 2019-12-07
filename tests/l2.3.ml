(* no taint *)

let something =
    print_string ( "hello" )

let secret_password _ = 
    "password12345"

let safe_data _ = 
    "this data is safe"

let secret_password =
    let distance = 1.00 in 
    let _ = if distance > 0.4 then 
        print_string "greater" 
    else print_string "lesser" in 
    print_string ( safe_data () )
