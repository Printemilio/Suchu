open Ast
open Runtime


exception Return_signal of value

(* Loop control uses exceptions like Return_signal does, so it unwinds nested
   blocks without every construct having to thread a status value around. Both
   are caught by the innermost enclosing loop. *)
exception Break_signal
exception Continue_signal


type interpreter = {
  globals : environment;
  mutable env : environment;
  mutable current_dir : string;
  module_cache : (string, value) Hashtbl.t;
  loading_modules : (string, unit) Hashtbl.t;
  embedded_modules : (string, string) Hashtbl.t;
}

(* --- Optional backend hooks -------------------------------------------------

   The evaluator carries no GUI dependency so it can be compiled to JavaScript
   with js_of_ocaml. Gui_backend installs the real implementations at startup;
   in a build without it, a [window] statement raises instead of opening one. *)

let gui_hook : (interpreter -> gui_block -> value) ref =
  ref (fun _ _ ->
      raise (Runtime_error "GUI support is not available in this build"))

(* Builtins contributed by optional backends, applied by [create]. *)
let extra_builtins : (interpreter -> unit) list ref = ref []


let with_scope interp env f =
  let previous = interp.env in
  interp.env <- env;
  Fun.protect ~finally:(fun () -> interp.env <- previous) f


let module_alias module_spec =
  let basename = Filename.basename module_spec in
  let stem =
    if Filename.check_suffix basename ".suchu" then Filename.chop_suffix basename ".suchu"
    else basename
  in
  String.map
    (fun ch ->
      match ch with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_' -> ch
      | _ -> '_')
    stem

let resolve_module_path current_dir module_spec =
  let with_extension =
    if Filename.extension module_spec = "" then module_spec ^ ".suchu" else module_spec
  in
  if Filename.is_relative with_extension then Filename.concat current_dir with_extension
  else with_extension


let rec run_program interp (program : program) =
  let result = ref V_null in
  List.iter (fun stmt -> result := execute interp stmt) program;
  !result

and execute interp = function
  | Import module_spec ->
      let alias = module_alias module_spec in
      if String.equal alias "" then raise (Runtime_error "Import module name cannot be empty");
      let module_value =
        match find_native_module module_spec with
        | Some bindings ->
            let module_env = child_env interp.globals in
            List.iter (fun (name, value) -> env_define module_env name value) bindings;
            V_module module_env
        | None ->
            let path = resolve_module_path interp.current_dir module_spec in
            (match Hashtbl.find_opt interp.module_cache path with
            | Some value -> value
            | None ->
                if Hashtbl.mem interp.loading_modules path then
                  raise
                    (Runtime_error
                       (Printf.sprintf "Circular import detected while loading '%s'" module_spec));
                let embedded_source = Hashtbl.find_opt interp.embedded_modules path in
                if embedded_source = None && not (Sys.file_exists path) then
                  raise
                    (Runtime_error
                       (Printf.sprintf "Cannot import '%s': file not found at %s" module_spec path));
                Hashtbl.add interp.loading_modules path ();
                Fun.protect
                  ~finally:(fun () -> Hashtbl.remove interp.loading_modules path)
                  (fun () ->
                    let source =
                      match embedded_source with
                      | Some source -> source
                      | None ->
                          let channel =
                            try open_in_bin path with
                            | Sys_error message ->
                                raise
                                  (Runtime_error
                                     (Printf.sprintf "Cannot import '%s': %s" module_spec message))
                          in
                          Fun.protect
                            ~finally:(fun () -> close_in channel)
                            (fun () -> really_input_string channel (in_channel_length channel))
                    in
                    let program =
                      try Parser.parse source with
                      | Lexical.Lexing_error message ->
                          raise
                            (Runtime_error
                               (Printf.sprintf "In imported module '%s': %s" module_spec message))
                      | Parser.Parse_error message ->
                          raise
                            (Runtime_error
                               (Printf.sprintf "In imported module '%s': %s" module_spec message))
                    in
                    let module_env = child_env interp.globals in
                    let previous_dir = interp.current_dir in
                    interp.current_dir <- Filename.dirname path;
                    let value =
                      Fun.protect
                        ~finally:(fun () -> interp.current_dir <- previous_dir)
                        (fun () ->
                          ignore (with_scope interp module_env (fun () -> run_program interp program));
                          V_module module_env)
                    in
                    Hashtbl.replace interp.module_cache path value;
                    value))
      in
      env_define interp.env alias module_value;
      module_value
  | FunctionDef decl ->
      let fn = V_function { params = decl.params; body = decl.body; env = interp.env } in
      env_define interp.env decl.name fn;
      V_null
  | ExprStmt expr -> evaluate interp expr
  | Assign (target, value_expr) ->
      let value = evaluate interp value_expr in
      assign interp target value;
      value
  | Return maybe_expr ->
      let value = Option.map (evaluate interp) maybe_expr |> Option.value ~default:V_null in
      raise (Return_signal value)
  | IfStmt (condition, then_block, else_block) ->
      if truthy (evaluate interp condition) then execute_block interp then_block
      else (
        match else_block with
        | Some block -> execute_block interp block
        | None -> V_null)
  | MatchStmt (target_expr, branches, default_block) ->
      let target_value = evaluate interp target_expr in
      let rec dispatch = function
        | [] -> (
            match default_block with
            | Some block -> execute_block interp block
            | None -> V_null)
        | (case_expr, case_block) :: rest ->
            let case_value = evaluate interp case_expr in
            if value_equals target_value case_value then execute_block interp case_block else dispatch rest
      in
      dispatch branches
  | Break -> raise Break_signal
  | Continue -> raise Continue_signal
  | TryStmt (attempted, binding, handler) -> (
      try execute_block interp attempted with
      | Runtime_error message ->
          (* A record, not a bare string, so more fields can be added later
             without breaking handlers that already read .message *)
          let error = V_record (ref [ ("message", V_string message) ]) in
          let handler_env = child_env interp.env in
          Option.iter (fun name -> env_define handler_env name error) binding;
          with_scope interp handler_env (fun () -> execute_block interp handler))
  | WhileStmt (condition, body) ->
      (try
         while truthy (evaluate interp condition) do
           try ignore (execute_block interp body) with Continue_signal -> ()
         done
       with Break_signal -> ());
      V_null
  | ForStmt (name, iterable_expr, body) ->
      let iterable = evaluate interp iterable_expr in
      let values = collect_iterable iterable in
      let loop_env = child_env interp.env in
      with_scope interp loop_env (fun () ->
          try
            List.iter
              (fun value ->
                if env_has_local loop_env name then env_assign loop_env name value
                else env_define loop_env name value;
                try ignore (execute_block interp body) with Continue_signal -> ())
              values
          with Break_signal -> ());
      V_null
  | Gui block -> !gui_hook interp block

and execute_block interp statements =
  let block_env = child_env interp.env in
  with_scope interp block_env (fun () ->
      let result = ref V_null in
      try
        List.iter (fun stmt -> result := execute interp stmt) statements;
        !result
      with Return_signal value -> raise (Return_signal value))


and assign interp target value =
  match target with
  | Identifier name -> (
      try
        ignore (env_lookup interp.env name);
        env_assign interp.env name value
      with
      | Runtime_error _ -> env_define interp.env name value)
  | Attribute (Identifier "Length", name) ->
      raise (Runtime_error (Printf.sprintf "Cannot assign to Length.%s" name))
  (* items[0] = x and user["name"] = x *)
  | Index (target, index) ->
      let container = evaluate interp target in
      let key = evaluate interp index in
      index_set container key value
  (* user.age = 22, and user.city = "Paris" to add a field. Records are
     references, so mutating one is visible through every name bound to it. *)
  | Attribute (target, name) -> (
      match evaluate interp target with
      | V_record fields -> record_set fields name value
      | other ->
          raise
            (Runtime_error
               (Printf.sprintf "Cannot assign field '%s' on a %s" name (type_of_value other))))
  | _ -> raise (Runtime_error "Invalid assignment target")

and evaluate interp = function
  | Int n -> V_int n
  | Float f -> V_float f
  | Bool b -> V_bool b
  | String s -> V_string s
  | Identifier name -> env_lookup interp.env name
  | List entries ->
      let values = entries |> List.map (evaluate interp) in
      V_list (ref values)
  | Index (target, index) ->
      let container = evaluate interp target in
      index_get container (evaluate interp index)
  | Record entries ->
      (* Later fields win, so { a: 1; a: 2; } holds 2 without duplicating 'a'. *)
      let fields = ref [] in
      List.iter (fun (name, expr) -> record_set fields name (evaluate interp expr)) entries;
      V_record fields
  | Set entries ->
      let values = entries |> List.map (evaluate interp) in
      make_set values
  | Null -> V_null
  | FunctionLiteral literal ->
      V_function { params = literal.params; body = literal.body; env = interp.env }
  | Unary (op, expr) ->
      let operand = evaluate interp expr in
      evaluate_unary op operand
  | Postfix (op, expr) ->
      evaluate_postfix interp op expr
  | Binary (left, op, right) ->
      let lv = evaluate interp left in
      let rv = evaluate interp right in
      evaluate_binary interp op lv rv
  | Call (callee_expr, arguments) ->
      let callee = evaluate interp callee_expr in
      let args = List.map (evaluate interp) arguments in
      call interp callee args
  | Attribute (expr, name) ->
      let obj = evaluate interp expr in
      evaluate_attribute interp obj name
  | IfExpr (cond, then_expr, else_expr) ->
      if truthy (evaluate interp cond) then evaluate interp then_expr else evaluate interp else_expr
  | Range (start_expr, end_expr) ->
      let start = evaluate interp start_expr |> expect_int in
      let finish = evaluate interp end_expr |> expect_int in
      let step = if finish >= start then 1 else -1 in
      let rec build acc current =
        if step > 0 && current > finish then List.rev acc
        else if step < 0 && current < finish then List.rev acc
        else build (current :: acc) (current + step)
      in
      V_list (ref (build [] start |> List.map (fun n -> V_int n)))

and evaluate_unary op operand =
  match op with
  | U_neg -> (
      match operand with
      | V_int n -> V_int (-n)
      | V_float f -> V_float (-.f)
      | _ -> raise (Runtime_error "Unary '-' expects a number"))
  | U_pos -> (
      match operand with
      | V_int _
      | V_float _ -> operand
      | _ -> raise (Runtime_error "Unary '+' expects a number"))
  | U_not -> V_bool (not (truthy operand))
  | U_pull -> (
      match operand with
      | V_list items -> (
          match List.rev !(items) with
          | [] -> raise (Runtime_error "Cannot pull from empty list")
          | head :: tail_rev ->
              items := List.rev tail_rev;
              head)
      | _ -> raise (Runtime_error "Prefix '@' expects a list"))

and evaluate_postfix interp op expr =
  match expr with
  | Identifier name ->
      let current = env_lookup interp.env name in
      let next =
        match (op, current) with
        | Post_inc, V_int n ->
            env_assign interp.env name (V_int (n + 1));
            V_int n
        | Post_dec, V_int n ->
            env_assign interp.env name (V_int (n - 1));
            V_int n
        | Post_inc, V_float f ->
            env_assign interp.env name (V_float (f +. 1.0));
            V_float f
        | Post_dec, V_float f ->
            env_assign interp.env name (V_float (f -. 1.0));
            V_float f
        | _ -> raise (Runtime_error "Postfix operator expects numeric variable")
      in
      next
  | _ -> raise (Runtime_error "Postfix operators require an identifier")

and evaluate_binary interp op left right =
  match op with
  | B_plus -> plus_operator left right
  | B_minus -> minus_operator left right
  | B_mul -> multiply_operator left right
  | B_div -> divide_operator left right
  | B_mod -> modulo_operator left right
  | B_pow -> pow_operator left right
  | B_union -> set_union left right
  | B_intersection -> set_intersection left right
  | B_eq -> V_bool (value_equals left right)
  | B_neq -> V_bool (not (value_equals left right))
  | B_lt -> compare_operator ( < ) left right
  | B_le -> compare_operator ( <= ) left right
  | B_gt -> compare_operator ( > ) left right
  | B_ge -> compare_operator ( >= ) left right
  | B_and ->
      if truthy left then right else left
  | B_or ->
      if truthy left then left else right
  | B_at -> (
      match left with
      | V_list items ->
          let _ =
            match right with
            | V_list other ->
                items := !(items) @ !(other);
                ()
            | _ -> items := !(items) @ [ right ]
          in
          V_list items
      | V_string text ->
          V_string (text ^ value_to_string right)
      | _ -> raise (Runtime_error "'@' operator expects list or string on left"))

and evaluate_attribute interp obj name =
  let expect_no_args method_name args =
    match args with
    | [] -> ()
    | _ -> raise (Runtime_error (Printf.sprintf "%s() expects no arguments" method_name))
  in
  let expect_one_arg method_name args =
    match args with
    | [ value ] -> value
    | _ -> raise (Runtime_error (Printf.sprintf "%s() expects one argument" method_name))
  in
  let expect_two_args method_name args =
    match args with
    | [ a; b ] -> (a, b)
    | _ -> raise (Runtime_error (Printf.sprintf "%s() expects two arguments" method_name))
  in
  match obj with
  | V_module module_env -> env_lookup module_env name
  | V_length -> length_of interp.env name
  | V_record fields -> (
      match record_get fields name with
      | Some value -> value
      | None -> raise (Runtime_error (Printf.sprintf "Record has no field '%s'" name)))
  | V_list items -> (
      match name with
      | "reverse" ->
          V_native (fun args ->
              expect_no_args "reverse" args;
              items := List.rev !(items);
              V_list items)
      | "sort" ->
          V_native (fun args ->
              expect_no_args "sort" args;
              items := List.sort compare_values !(items);
              V_list items)
      | "clear" ->
          V_native (fun args ->
              expect_no_args "clear" args;
              items := [];
              V_list items)
      | "sum" ->
          V_native (fun args ->
              expect_no_args "sum" args;
              let (int_total, float_total_opt) =
                List.fold_left
                  (fun (int_acc, float_opt) value ->
                    match (value, float_opt) with
                    | V_int n, None -> (int_acc + n, None)
                    | V_int n, Some f -> (0, Some (f +. float_of_int n))
                    | V_float f, None -> (0, Some (float_of_int int_acc +. f))
                    | V_float f, Some acc -> (0, Some (acc +. f))
                    | _ -> raise (Runtime_error "sum() expects numeric elements"))
                  (0, None) !(items)
              in
              match float_total_opt with
              | Some total -> V_float total
              | None -> V_int int_total)
      | "pop" ->
          V_native (fun args ->
              match args with
              | [] -> (
                  match List.rev !(items) with
                  | [] -> raise (Runtime_error "pop() on empty list")
                  | head :: tail_rev ->
                      items := List.rev tail_rev;
                      head)
              | [ index ] ->
                  let idx = expect_int index in
                  let rec extract i acc remaining =
                    match remaining with
                    | [] -> raise (Runtime_error "pop() index out of bounds")
                    | hd :: tl ->
                        if i = 0 then
                          (List.rev acc @ tl, hd)
                        else
                          extract (i - 1) (hd :: acc) tl
                  in
                  let updated, value = extract idx [] !(items) in
                  items := updated;
                  value
              | _ -> raise (Runtime_error "pop() expects zero or one argument"))
      | _ -> raise (Runtime_error (Printf.sprintf "Unknown list attribute '%s'" name)))
  | V_string text -> (
      match name with
      | "split" ->
          V_native (fun args ->
              let delimiter = expect_string "split" (expect_one_arg "split" args) in
              let parts = split_string text delimiter |> List.map (fun part -> V_string part) in
              V_list (ref parts))
      | "join" ->
          V_native (fun args ->
              let list_value = expect_one_arg "join" args in
              let list_ref = expect_list "join" list_value in
              let parts =
                !(list_ref)
                |> List.map (function V_string s -> s | _ -> raise (Runtime_error "join() expects strings"))
              in
              V_string (String.concat text parts))
      | "replace" ->
          V_native (fun args ->
              let old_v, new_v = expect_two_args "replace" args in
              let old_sub = expect_string "replace" old_v in
              let new_sub = expect_string "replace" new_v in
              V_string (replace_substring text old_sub new_sub))
      | "startswith" ->
          V_native (fun args ->
              let prefix = expect_string "startswith" (expect_one_arg "startswith" args) in
              V_bool (starts_with ~prefix text))
      | "endswith" ->
          V_native (fun args ->
              let suffix = expect_string "endswith" (expect_one_arg "endswith" args) in
              V_bool (ends_with ~suffix text))
      | _ -> raise (Runtime_error (Printf.sprintf "Unknown string attribute '%s'" name)))
  | V_set items -> (
      match name with
      | "clear" ->
          V_native (fun args ->
              expect_no_args "clear" args;
              items := [];
              V_set items)
      | _ -> raise (Runtime_error (Printf.sprintf "Unknown set attribute '%s'" name)))
  | _ -> raise (Runtime_error "Unknown attribute access")

and call interp callee args =
  match callee with
  | V_native fn -> fn args
  | V_function fn ->
      if List.length args <> List.length fn.params then
        raise
          (Runtime_error
             (Printf.sprintf "Function expected %d arguments but received %d" (List.length fn.params)
                (List.length args)));
      let local_env = child_env fn.env in
      List.iter2 (env_define local_env) fn.params args;
      with_scope interp local_env (fun () ->
          try
            (* execute body statements *)
            let result = ref V_null in
            List.iter (fun stmt -> result := execute interp stmt) fn.body;
            !result
          with Return_signal value -> value)
  | _ -> raise (Runtime_error "Attempted to call a non-callable value")


and collect_iterable = function
  | V_list items -> !(items)
  | V_set items -> !(items)
  (* Iterating a record yields its field names, in declaration order. *)
  | V_record fields -> List.map (fun (name, _) -> V_string name) !fields
  | V_string text ->
      let chars = List.init (String.length text) (fun i -> V_string (String.make 1 text.[i])) in
      chars
  | V_int _
  | V_float _
  | V_bool _
  | V_null
  | V_native _
  | V_function _
  | V_length
  | V_module _
  | V_object _ -> raise (Runtime_error "Value is not iterable")

let register_interpreter_builtins interp =
  let define name fn = env_define interp.globals name (V_native fn) in
  define "map"
    (function
      | [ fn; list_value ] ->
          let list_ref = expect_list "map" list_value in
          let mapped = !(list_ref) |> List.map (fun item -> call interp fn [ item ]) in
          V_list (ref mapped)
      | _ -> raise (Runtime_error "map expects (function, list)"));
  define "filter"
    (function
      | [ fn; list_value ] ->
          let list_ref = expect_list "filter" list_value in
          let filtered =
            !(list_ref)
            |> List.filter (fun item -> truthy (call interp fn [ item ]))
          in
          V_list (ref filtered)
      | _ -> raise (Runtime_error "filter expects (function, list)"));
  define "reduce"
    (function
      | [ fn; list_value ] ->
          let list_ref = expect_list "reduce" list_value in
          (match !(list_ref) with
          | [] -> raise (Runtime_error "reduce() of empty list")
          | head :: tail ->
              List.fold_left (fun acc item -> call interp fn [ acc; item ]) head tail)
      | [ fn; list_value; initial ] ->
          let list_ref = expect_list "reduce" list_value in
          List.fold_left (fun acc item -> call interp fn [ acc; item ]) initial !(list_ref)
      | _ -> raise (Runtime_error "reduce expects (function, list[, initial])"));
  define "eval"
    (function
      | [ V_string source ] ->
          let program = Parser.parse source in
          run_program interp program
      | _ -> raise (Runtime_error "eval expects a source string"))

let create ?(base_dir = Sys.getcwd ()) ?(embedded_modules = []) () =
  Suchu_stdlib.register ();
  let globals = create_global_environment () in
  let embedded_table = Hashtbl.create (max 8 (List.length embedded_modules)) in
  List.iter (fun (path, source) -> Hashtbl.replace embedded_table path source) embedded_modules;
  let interp =
    {
      globals;
      env = globals;
      current_dir = base_dir;
      module_cache = Hashtbl.create 16;
      loading_modules = Hashtbl.create 8;
      embedded_modules = embedded_table;
    }
  in
  register_interpreter_builtins interp;
  List.iter (fun register -> register interp) !extra_builtins;
  interp
