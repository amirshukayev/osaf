open Stdio

type cfg = Node of Typedtree.expression * cfg list

let filename = "test2.ml"

let secret_password _ =
    "my_password12345"

let debug = true

let rec add_args_to_cfg args = 
    match args with 
    | [] -> None 
    | hd :: tl -> 
        let rest_of_cfg = add_args_to_cfg tl in 
        (match rest_of_cfg with 
        | Some _cfg -> Some(Node(hd, _cfg::[]))
        | None -> Some(Node(hd, [])))

and build_up_cfg expression : cfg =
    match expression.Typedtree.exp_desc with 
    | Texp_ident _ -> Node (expression, []) (* cfg is done this way *)
    | Texp_apply _ -> Node(expression, [])
    | _ -> Node(expression, [])

(* let types_to_function = Hashtbl.create (40) *)

let get_potential_next_fns apply_expr htbl =
    Hashtbl.find_all htbl apply_expr.Typedtree.exp_type

let debug_print_string str = 
    match debug with 
    | true -> print_string str 
    | false -> ()

let debug_print_int value = 
    match debug with 
    | true -> print_int value 
    | false -> ()

let parse_regular source_string =
    source_string 
    |> Lexing.from_string
    |> Parse.implementation

let parse_pparse source_string = 
     source_string

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
TODO: figure out inter proceduralism *)
let rec does_expression_generate_taint expression =
    let _ = debug_print_string "checking if expression generates taint" in
    match expression.Typedtree.exp_desc with
    | Texp_let _ -> true
    | Texp_ident (access_path, _, _) -> 
        let last = Path.last access_path in 
        let is_taint = Str.string_match secret_pattern last 0 in 
        let _ = print_string last in 
        let _ = print_string (if is_taint then "  taint\n" else "  not taint\n") in 
        let _ = debug_print_string last in
        let _ = print_string "\n" in
        is_taint
    | Texp_apply (expr, _) -> does_expression_generate_taint expr (* todo: figure this out *)
    | _ -> false

and f_vbinding vbinding = 
    match (does_expression_generate_taint vbinding.Typedtree.vb_expr) with
    | true -> (match vbinding.Typedtree.vb_pat.Typedtree.pat_desc with 
        | Tpat_var (ident, _) -> 
            print_string "IDENT: ";
            print_string (Ident.name ident);
            print_string (secret_password ());
            print_newline ();
            Some (Ident.name ident)
        | _ -> None)
    | false -> None

(* and tainted_vars vbinding already_tainted = 
    match (does_expression_generate_taint vbinding.Typedtree.vb_expr
        || ) *)
    
(* given an expression
return a list of all unqie 
variables names tainted by this expression *)
and get_tainted_vars expression = 
    match expression.Typedtree.exp_desc with 
    | Texp_let (_, vbindings, _) -> 
        List.filter_map f_vbinding vbindings
    | _ -> []

(* wrapper to convert from the list of 
optional function args *)
let wrap_does_expr_generate_taint func_arg_exp = 
    match func_arg_exp with 
    | _, Some expr -> does_expression_generate_taint expr
    | _ -> false

(* method sink is just denoted by your name 
this is configurable from the regex pattern 
at the top of the file *)
let is_method_sink_with_taint expression =
    match expression.Typedtree.exp_desc with
    | Texp_apply (func_expression, list_of_args) -> 
        let is_sink = is_expression_sink func_expression in 
        (match is_sink with 
        | true -> 
            let _ = debug_print_string "is a sink\n" in
            let list_of_rets = List.map wrap_does_expr_generate_taint list_of_args in 
            List.mem true list_of_rets
        | false -> false)
    | _ -> false

(* recursive iteration through a structure node 
in an ocaml program *)
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
                    let _ = debug_print_string hd.Parsetree.pvb_loc.loc_start.pos_fname in
                    let _ = debug_print_string " " in
                    let _ = debug_print_int (hd.Parsetree.pvb_loc.loc_start.pos_lnum) in
                    let _ = debug_print_string "\n" in
                    prec tl
                in
            let _ = prec value_bindings in
            eval_recurse tl
        | _ ->
            eval_recurse tl

(* for signature types *)
let print_sigitem (sigitem : Types.signature_item) =
    match sigitem with
    | Sig_value (identifier, _, _) ->
        let _ = debug_print_string "checkpoint psi.02\n" in
        if debug 
            then Ident.print Format.std_formatter identifier
            else ()
    | _ ->
        debug_print_string "something else"

(* print the elemtns of a type *)
let rec print_type_signature (sign : Types.signature) =
    match sign with
    | [] -> ()
    | hd :: tl ->
        let _ = print_sigitem hd in
        print_type_signature tl

(* process a value binding function 
note value bindings are pairs of what you are setting,
and what you are setting it to *)
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
    | Texp_function _ -> if debug then print_string "_function\n" else ()
    | Texp_apply _ -> if debug then print_string "_apply\n" else ()
    | Texp_match _ -> if debug then print_string "_match\n" else ()
    | Texp_tuple _ -> if debug then print_string "_tuple\n" else ()
    | Texp_field _ -> if debug then print_string "_field\n" else ()
    | _ -> debug_print_string "\thuh\n"

(* given any expression, return true if security issue is found else false
current security issues defined as a taint var reaching a sink
for the types of expressions
| idenities -> see if its a sink, and if it is applied on a taint
| function applications -> recursively check if any children are *)
let rec check_expr_for_security_issues (expr: Typedtree.expression) : bool =
    is_method_sink_with_taint expr ||
    match expr.Typedtree.exp_desc with 
    | Texp_ident _ -> is_method_sink_with_taint expr
    | Texp_let (_, vbl, expr) -> 
        let _ = if debug then print_string "Texp_let\n" 
            else () in
        List.map check_value_binding_for_security_issues vbl
        |> List.mem true 
        || check_expr_for_security_issues expr
    | Texp_apply (inner_expr, arg_list) ->
        let _ = if debug then print_string "Texp_apply\n" 
            else () in
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

(* structures are the top level of an ocaml file 
generally consist of several expressions, that could be
separated with ;
LOOK INTO THIS MORE*)
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
    | Tstr_eval _ -> if debug then print_string "one of these\n" else ()
    | Tstr_value (_, vbinding_list) -> 
        let _ = List.map vbinding_func vbinding_list in ()
    | Tstr_primitive _ -> 
        if debug then print_string "3\n"
        else ()
    | Tstr_type _ -> 
        if debug then print_string "4\n"
        else ()
    | Tstr_attribute _ -> 
        if debug then print_string "5\n"
        else ()
    | _ -> 
        if debug then print_string "not one of these\n"
        else ()

let rec print_type_tree (tt : Typedtree.structure_item list) =
    match tt with
    | [] -> ()
    | hd :: tl ->
        let _ = print_structure_item hd in 
        print_type_tree tl

let analyze_tt ttstruct =
    print_type_signature ttstruct.Typedtree.str_type

(* modified version of above to work with fact
propogation 
currently bugged*)
let is_method_sink_with_taint3 (_ : Ident.t list) expression =
    match expression.Typedtree.exp_desc with
    | Texp_apply (func_expression, list_of_args) -> 
        let is_sink = is_expression_sink func_expression in 
        (match is_sink with 
        | true -> 
            let _ = print_string "is a sink\n" in
            let list_of_rets = List.map wrap_does_expr_generate_taint list_of_args in 
            let list_of_rets2 = List.map (
                fun (arg) -> (
                    (match arg with 
                    | _, Some expr ->
                        (match expr.Typedtree.exp_desc with 
                        | Texp_ident (path, _, _) -> 
                            let last = Path.last path in 
                            Str.string_match secret_pattern last 0
                        | Texp_apply (_, _) -> 
                            false
                        | _ -> false
                        )
                    | _ -> false)
                )
            ) list_of_args in
            List.mem true list_of_rets || List.mem true list_of_rets2
        | false -> false)
    | _ -> false

(* works with fact propogation
currently the best working version *)
let is_method_sink_with_taint2 tainted_vars expression =
    match expression.Typedtree.exp_desc with
    | Texp_apply (func_expression, list_of_args) -> 
        let is_sink = is_expression_sink func_expression in 
        (match is_sink with 
        | true -> 
            let _ = debug_print_string "is a sink\n" in
            let list_of_rets = List.map (fun (arg) -> (
                match arg with 
                | _, Some expr -> 
                    (match expr.Typedtree.exp_desc with 
                    | Texp_ident (path, _, _) -> 
                        print_string (Path.last path);
                        List.mem (Path.last path) tainted_vars
                    | _ -> false
                    )
                | _ -> false
            )) list_of_args in 
            List.mem true list_of_rets
        | false -> false)
    | _ -> false

(* expression security checks with fact propagation
currently best that works *)
let rec check_expr_for_security_issues2 tainted_vars (expr: Typedtree.expression) : bool =
    is_method_sink_with_taint2 tainted_vars expr ||
    match expr.Typedtree.exp_desc with 
    | Texp_ident _ -> is_method_sink_with_taint2 tainted_vars expr
    | Texp_let (_, vbl, expr) -> 
        let tainted_vars = tainted_vars @
            (List.filter_map f_vbinding vbl) in
        List.map (check_vb_for_sec_issues2 tainted_vars) vbl
        |> List.mem true 
        || check_expr_for_security_issues2 tainted_vars expr
    | Texp_apply (inner_expr, arg_list) ->
        let _ = if debug then print_string "Texp_apply\n"
            else () in
        is_method_sink_with_taint expr 
        || check_expr_for_security_issues inner_expr
        || List.map (fun (arg_item) ->
            match arg_item with
            | _, Some expr -> check_expr_for_security_issues expr
            | _ -> false) arg_list
            |> List.mem true
    | _ -> false

(* value binding security checks with fact propagation
currently best that works 
value bindings are as follows
let x = 1
and y = 2
and x = (fun (a b) -> a <. b 
three value binds with path, expression*)
and check_vb_for_sec_issues2 tainted_vars vb = 
    is_method_sink_with_taint2 tainted_vars vb.Typedtree.vb_expr
    || check_expr_for_security_issues2 tainted_vars vb.Typedtree.vb_expr

let rec taint_analysis2 tts tainted_vars = 
    match tts with 
    | [] -> false 
    | hd :: tl -> (* update tainted vars and continue *)
        let tainted_vars = tainted_vars @ 
            (match hd.Typedtree.str_desc with 
            | Tstr_value (_, vbindings) -> 
                let retval = List.filter_map f_vbinding vbindings in 
                let _ = if List.length retval > 0 then print_string "something" else () in 
                retval
            | _ -> []) in
        match hd.Typedtree.str_desc with 
        | Tstr_value (_, vbl) -> 
            (List.map (check_vb_for_sec_issues2 tainted_vars) vbl
            |> List.mem true)
            || taint_analysis2 tl tainted_vars
        | Tstr_eval (expr, _) -> 
            check_expr_for_security_issues2 tainted_vars expr
            || taint_analysis2 tl tainted_vars 
        | _ -> taint_analysis2 tl tainted_vars
        
let () =
    let _ = print_string "Examining the following code:\n" in
    let _ = print_string Sys.argv.(1) in
    let _ = print_newline in
    let _ = print_string (In_channel.read_all Sys.argv.(1)) in
    let _ = print_string "\n\n" in
    let preprocessed_file = Pparse.preprocess Sys.argv.(1) in 
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
    let _ = print_newline in 
    let _ = print_string "analysis 2 result: " in 
    let _ = print_string (if taint_analysis2 tts.Typedtree.str_items [] then "true" else "false") in
    print_newline ()