let secret_filename = "secrets.txt"
let number = 123

let secret_read () = 
    let _ = 1 in
    secret_filename

let seemingly_safe = secret_read()

let () =
    let _ = secret_read () in
    let _ = print_int number in
    print_string (seemingly_safe);
