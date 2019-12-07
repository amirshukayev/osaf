(* example *)

let transform_pos v = v +. 1.0
let transform_var v dx = v +. dx

let apply_transform tformation value = 
    tformation value
    
let () = 
    print_float (apply_transform transform_pos 4.00)
