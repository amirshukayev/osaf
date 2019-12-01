open Stdio

let filename = "test1.ml"

let parse_regular source_string =
    source_string 
    |> Lexing.from_string
    |> Parse.implementation;;

let parse_pparse source_string = 
     source_string;;

let e =
    Compmisc.init_path false;
    Compmisc.initial_env ()

(* how we will determine if an identity returns
tainted data, basically if it is a function or
something that starts with secret_ 
ex: secret_data *)
let secret_pattern = 
    Str.regexp "secret_"

(* how we will determine if an identity is a sink
for tainted information. Basically if it is a print
function, so starts with print_
ex: print_string *)
let sink_pattern = 
    Str.regexp "print_"

let is_struct_item_secret struct_item =
    match struct_item.Typedtree.str_desc with
    | Tstr_value (_, hd::_) -> 
        (match hd.Typedtree.vb_pat.Typedtree.pat_desc with
        | Tpat_var (key, _) ->
            let _ = print_string "Identity: " in
            let _ = print_string (Ident.name key) in
            Str.string_match secret_pattern (Ident.name key) 0
        | _ -> false)
    | _ -> false

let is_expression_sink expression = 
    match expression.Typedtree.exp_desc with
    | Texp_ident (access_path, _, _) -> 
        let last = Path.last access_path in 
        Str.string_match sink_pattern last 0
    | _ -> false

(*
recursively check an expression, 
and all of its inner experessions for a taint source
TODO: figure out inter proceduralism*)
let rec does_expression_generate_taint expression =
    let _ = "checking if expression generates taint" in
    match expression.Typedtree.exp_desc with
    | Texp_ident (access_path, _, _) -> 
        let last = Path.last access_path in 
        let is_taint = Str.string_match secret_pattern last 0 in 
        let _ = print_string (if is_taint then "taint\n" else "not taint\n") in 
        let _ = print_string last in
        let _ = print_string "\n" in
        is_taint
    | Texp_apply (expr, _) -> does_expression_generate_taint expr(* todo: figure this out *)
    | _ -> false

let wrap_does_expr_generate_taint func_arg_exp = 
    match func_arg_exp with 
    | _, Some expr -> does_expression_generate_taint expr
    | _ -> false

let is_method_sink_with_taint expression =
    match expression.Typedtree.exp_desc with
    | Texp_apply (func_expression, list_of_args) -> 
        let is_sink = is_expression_sink func_expression in 
        (match is_sink with 
        | true -> 
            let _ = print_string "is a sink\n" in
            let list_of_rets = List.map wrap_does_expr_generate_taint list_of_args in 
            List.mem true list_of_rets
        | false -> false)
    | _ -> false


let rec eval_recurse our_structure =
    match our_structure with
    | [] -> ()
    | hd :: tl -> 
        let desc = hd.Parsetree.pstr_desc in 
        match desc with
        | Pstr_value (Asttypes.Nonrecursive, value_bindings) ->
            let _ = print_string "value statement\n" in
            let rec prec vbind_list = 
                match vbind_list with
                | [] -> ()
                | hd :: tl ->
                    let _ = print_string hd.Parsetree.pvb_loc.loc_start.pos_fname in
                    let _ = print_string " " in
                    let _ = print_int (hd.Parsetree.pvb_loc.loc_start.pos_lnum) in
                    let _ = print_string "\n" in
                    prec tl
                in
            let _ = prec value_bindings in
            eval_recurse tl
        | _ ->
            eval_recurse tl

let print_sigitem (sigitem : Types.signature_item) =
    match sigitem with
    | Sig_value (identifier, _, _) ->
        let _ = print_string "gonna print dis\n" in
        Ident.print Format.std_formatter identifier
    | _ ->
        print_string "something else"

let rec print_type_signature (sign : Types.signature) =
    match sign with
    | [] -> ()
    | hd :: tl ->
        let _ = print_sigitem hd in
        print_type_signature tl

let rec vbinding_func item = 
    match item.Typedtree.vb_expr.Typedtree.exp_desc with
    | Texp_constant _ -> print_string "_constant\n"
    | Texp_let (_, vb_list, _) -> 
        let _ = print_string "_let\n" in
        let _ = match vb_list with
        | [] -> print_string "empty list\n"
        | vbl -> 
            let _ = List.map vbinding_func vbl in ()
        in ()
    | Texp_function _ -> print_string "_function\n"
    | Texp_apply _ -> print_string "_apply\n"
    | Texp_match _ -> print_string "_match\n"
    | Texp_tuple _ -> print_string "_tuple\n"
    | Texp_field _ -> print_string "_field\n"
    | _ -> print_string "\thuh\n"




let rec check_expr_for_security_issues (expr: Typedtree.expression) : bool =
    is_method_sink_with_taint expr ||
    match expr.Typedtree.exp_desc with 
    | Texp_ident _ -> is_method_sink_with_taint expr
    | Texp_let (_, vbl, expr) -> 
        let _ = print_string "Texp_let\n" in
        List.map check_value_binding_for_security_issues vbl
        |> List.mem true 
        || check_expr_for_security_issues expr
    | Texp_apply (inner_expr, arg_list) ->
        let _ = print_string "Texp_apply\n" in
        is_method_sink_with_taint expr 
        || check_expr_for_security_issues inner_expr
        || List.map (fun (arg_item) ->
            match arg_item with
            | _, Some expr -> check_expr_for_security_issues expr
            | _ -> false) arg_list
            |> List.mem true
    | _ -> false

and check_value_binding_for_security_issues (vbinding : Typedtree.value_binding) : bool =
    is_method_sink_with_taint vbinding.Typedtree.vb_expr
    || check_expr_for_security_issues vbinding.Typedtree.vb_expr

let check_structure_item_for_security_issues struct_item =
    match struct_item.Typedtree.str_desc with 
    | Tstr_eval (expr, _) -> 
        let _ = print_string "Tstr_eval" in
        check_expr_for_security_issues expr
    | Tstr_value (_, vbinding_list) -> 
        let _ = print_string "Tstr_value\n" in
        List.map check_value_binding_for_security_issues vbinding_list
        |> List.mem true
    | _ -> false 

let print_structure_item struct_item =
    let _ = print_string (if (is_struct_item_secret struct_item) then " true " else " false ") in
    match struct_item.Typedtree.str_desc with
    | Tstr_eval _ -> print_string "one of these YEAH\n"
    | Tstr_value (_, vbinding_list) -> 
        let _ = List.map vbinding_func vbinding_list in ()
    | Tstr_primitive _ -> print_string "3\n"
    | Tstr_type _ -> print_string "4\n"
    | Tstr_attribute _ -> print_string "5\n"
    | _ -> print_string "not one of these\n"

let rec print_type_tree (tt : Typedtree.structure_item list) =
    match tt with
    | [] -> ()
    | hd :: tl ->
        let _ = print_structure_item hd in 
        print_type_tree tl

let analyze_tt ttstruct =
    print_type_signature ttstruct.Typedtree.str_type

let () =
    let _ = print_string "Examining the following code:\n" in
    let _ = print_string (In_channel.read_all filename) in
    let _ = print_string "\n\n" in
    let preprocessed_file = Pparse.preprocess filename in 
    let ast = Pparse.parse_implementation ~tool_name:"amir" preprocessed_file in
    let _ = eval_recurse ast in
    let _ = print_string "\n----- next section -----\n" in
    (* let ttstruct, ttsig, ttsignames, env = Typemod.type_structure e ast Location.none in *)
    let tts, _, _, _ = Typemod.type_structure e ast Location.none in
    let _ = print_type_tree tts.Typedtree.str_items in
    let _ = print_string "\n--- taint analyis inbound ---\n" in
    let taint = List.map check_structure_item_for_security_issues tts.Typedtree.str_items
    |> List.mem true in 
    let _ = print_string "result of the SA: " in
    let _ = print_string (if taint then "true" else "false") in 
    let _ = print_string "\n" in
    print_string "\n"