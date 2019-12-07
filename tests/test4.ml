let secret_read () = 
    "secrets.txt"

let () =
    let value = 500 in 
    let _ = print_string "random stuff: " in 
    let ok = if value > 400 
        then secret_read()
        else "something safe" in 
    let _ = print_string "getting ready\n" in
    print_string ok
