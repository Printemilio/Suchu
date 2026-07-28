open Ast

type native_object = {
  kind : string;
  payload : Obj.t;
}

type value =
  | V_null
  | V_int of int
  | V_float of float
  | V_bool of bool
  | V_string of string
  | V_list of value list ref
  | V_set of value list ref
  (* An association list rather than a hashtable so fields keep insertion
     order: printing and JSON round-trips stay predictable, and records are
     small enough that lookup cost does not matter. *)
  | V_record of (string * value) list ref
  | V_function of function_value
  | V_native of native_function
  | V_module of environment
  | V_object of native_object
  | V_length

and function_value = {
  params : string list;
  body : block;
  env : environment;
}

and native_function = value list -> value

and environment = {
  table : (string, value) Hashtbl.t;
  parent : environment option;
}

exception Runtime_error of string

let empty_env () = { table = Hashtbl.create 32; parent = None }

let child_env parent = { table = Hashtbl.create 16; parent = Some parent }

let rec env_lookup env name =
  match Hashtbl.find_opt env.table name with
  | Some value -> value
  | None -> (
      match env.parent with
      | Some parent -> env_lookup parent name
      | None -> raise (Runtime_error (Printf.sprintf "Undefined variable '%s'" name)))

let rec env_assign env name value =
  if Hashtbl.mem env.table name then Hashtbl.replace env.table name value
  else
    match env.parent with
    | Some parent -> env_assign parent name value
    | None -> Hashtbl.replace env.table name value

let env_define env name value = Hashtbl.replace env.table name value

let env_has_local env name = Hashtbl.mem env.table name

let native_modules : (string, (string * value) list) Hashtbl.t = Hashtbl.create 16

let register_native_module name bindings =
  if String.equal (String.trim name) "" then
    raise (Invalid_argument "Native module name cannot be empty");
  Hashtbl.replace native_modules name bindings

let find_native_module name = Hashtbl.find_opt native_modules name

let make_object kind value = V_object { kind; payload = Obj.repr value }

let expect_object name expected_kind = function
  | V_object object_value when String.equal object_value.kind expected_kind ->
      Obj.obj object_value.payload
  | V_object object_value ->
      raise
        (Runtime_error
           (Printf.sprintf "%s expects a %s, got %s" name expected_kind object_value.kind))
  | _ -> raise (Runtime_error (Printf.sprintf "%s expects a %s" name expected_kind))

(* --- record helpers ------------------------------------------------------ *)

let record_get fields name = List.assoc_opt name !fields

let record_has fields name = List.mem_assoc name !fields

(* Updating keeps the field in place; a new field is appended, so the order a
   record is written in is the order it prints in. *)
let record_set fields name value =
  if record_has fields name then
    fields :=
      List.map (fun (key, old) -> if String.equal key name then (key, value) else (key, old)) !fields
  else fields := !fields @ [ (name, value) ]

let rec value_to_string = function
  | V_null -> "none"
  | V_int n -> string_of_int n
  | V_float f ->
      let s = string_of_float f in
      if String.contains s '.' then s else s ^ ".0"
  | V_bool true -> "true"
  | V_bool false -> "false"
  | V_string s -> s
  | V_list items ->
      let data = !(items) |> List.map value_to_string |> String.concat ", " in
      "[" ^ data ^ "]"
  | V_set items ->
      let data = !(items) |> List.map value_to_string |> String.concat ", " in
      "{" ^ data ^ "}"
  | V_record fields ->
      let data =
        !fields
        |> List.map (fun (name, value) -> name ^ ": " ^ value_to_string value)
        |> String.concat ", "
      in
      "{" ^ data ^ "}"
  | V_function _ -> "<function>"
  | V_native _ -> "<native>"
  | V_module _ -> "<module>"
  | V_object object_value -> "<" ^ object_value.kind ^ ">"
  | V_length -> "<Length>"

let truthy = function
  | V_null -> false
  | V_bool b -> b
  | V_int n -> n <> 0
  | V_float f -> f <> 0.0
  | V_string s -> s <> ""
  | V_list items -> !(items) <> []
  | V_set items -> !(items) <> []
  | V_record fields -> !fields <> []
  | V_function _ -> true
  | V_native _ -> true
  | V_module _ -> true
  | V_object _ -> true
  | V_length -> true

let expect_number value =
  match value with
  | V_int n -> (float_of_int n, `Int)
  | V_float f -> (f, `Float)
  | _ -> raise (Runtime_error "Expected numeric value")

let expect_int = function
  | V_int n -> n
  | _ -> raise (Runtime_error "Expected integer value")

let expect_list name value =
  match value with
  | V_list items -> items
  | _ -> raise (Runtime_error (Printf.sprintf "%s expects a list" name))

let expect_string name value =
  match value with
  | V_string s -> s
  | _ -> raise (Runtime_error (Printf.sprintf "%s expects a string" name))

let expect_set name value =
  match value with
  | V_set items -> items
  | _ -> raise (Runtime_error (Printf.sprintf "%s expects a set" name))

let rec value_equals left right =
  match (left, right) with
  | V_null, V_null -> true
  | V_int a, V_int b -> a = b
  | V_float a, V_float b -> a = b
  | V_int a, V_float b -> float_of_int a = b
  | V_float a, V_int b -> a = float_of_int b
  | V_bool a, V_bool b -> a = b
  | V_string a, V_string b -> String.equal a b
  | V_list a, V_list b -> List.length !(a) = List.length !(b) && List.for_all2 value_equals !(a) !(b)
  | V_set a, V_set b ->
      let left_items = !(a) in
      let right_items = !(b) in
      let contains lst value = List.exists (fun item -> value_equals item value) lst in
      List.length left_items = List.length right_items
      && List.for_all (fun item -> contains right_items item) left_items
      && List.for_all (fun item -> contains left_items item) right_items
  (* Field order is presentation, not identity: two records are equal when they
     hold the same names bound to equal values. *)
  | V_record a, V_record b ->
      List.length !a = List.length !b
      && List.for_all
           (fun (name, value) ->
             match List.assoc_opt name !b with
             | Some other -> value_equals value other
             | None -> false)
           !a
  | V_object a, V_object b ->
      String.equal a.kind b.kind && a.payload == b.payload
  | V_module a, V_module b -> a == b
  | V_length, V_length -> true
  | _ -> false

let normalize_set values =
  List.fold_left
    (fun acc item -> if List.exists (fun existing -> value_equals existing item) acc then acc else acc @ [ item ])
    [] values

let make_set values = V_set (ref (normalize_set values))

let set_union left right =
  match (left, right) with
  | V_set a, V_set b -> make_set (!(a) @ !(b))
  | _ -> raise (Runtime_error "Set union expects two sets")

let set_intersection left right =
  match (left, right) with
  | V_set a, V_set b ->
      let filtered =
        !(a) |> List.filter (fun value -> List.exists (fun other -> value_equals value other) !(b))
      in
      make_set filtered
  | _ -> raise (Runtime_error "Set intersection expects two sets")

let set_difference left right =
  match (left, right) with
  | V_set a, V_set b ->
      let filtered =
        !(a) |> List.filter (fun value -> not (List.exists (fun other -> value_equals value other) !(b)))
      in
      make_set filtered
  | _ -> raise (Runtime_error "Set difference expects two sets")

let numeric_binary op left right =
  let (lv, lt) = expect_number left in
  let (rv, rt) = expect_number right in
  match op (lv, lt) (rv, rt) with
  | `Int result -> V_int result
  | `Float result -> V_float result

let plus_operator left right =
  match (left, right) with
  | V_int a, V_int b -> V_int (a + b)
  | V_string a, V_string b -> V_string (a ^ b)
  | V_string a, v -> V_string (a ^ value_to_string v)
  | V_list items, V_list other ->
      items := !(items) @ !(other);
      V_list items
  | V_list items, v ->
      items := !(items) @ [ v ];
      V_list items
  | _ ->
      let (a, _), (b, _) = (expect_number left, expect_number right) in
      V_float (a +. b)

let minus_operator left right =
  match (left, right) with
  | V_int a, V_int b -> V_int (a - b)
  | V_set _, V_set _ -> set_difference left right
  | V_set _, _ -> raise (Runtime_error "Set difference expects a set on the right")
  | _ ->
      let (a, _), (b, _) = (expect_number left, expect_number right) in
      V_float (a -. b)

let multiply_operator left right =
  match (left, right) with
  | V_int a, V_int b -> V_int (a * b)
  | _ ->
      let (a, _), (b, _) = (expect_number left, expect_number right) in
      V_float (a *. b)

let divide_operator left right =
  let (a, _), (b, _) = (expect_number left, expect_number right) in
  V_float (a /. b)

let modulo_operator left right =
  match (left, right) with
  | V_int a, V_int b -> V_int (a mod b)
  | _ -> raise (Runtime_error "Modulo expects integer operands")

let pow_operator left right =
  let (a, _), (b, _) = (expect_number left, expect_number right) in
  V_float (a ** b)

let compare_operator cmp left right =
  let (a, _), (b, _) = (expect_number left, expect_number right) in
  V_bool (cmp a b)

let compare_values left right =
  match (left, right) with
  | V_int a, V_int b -> compare a b
  | V_float a, V_float b -> Float.compare a b
  | V_int a, V_float b -> Float.compare (float_of_int a) b
  | V_float a, V_int b -> Float.compare a (float_of_int b)
  | V_string a, V_string b -> String.compare a b
  | V_bool a, V_bool b -> Bool.compare a b
  | _ -> raise (Runtime_error "Values are not comparable")

let rec find_substring text pattern start =
  let text_len = String.length text in
  let pattern_len = String.length pattern in
  if pattern_len = 0 then Some start
  else if start + pattern_len > text_len then None
  else
    let rec matches i =
      if i = pattern_len then true
      else if text.[start + i] <> pattern.[i] then false
      else matches (i + 1)
    in
    if matches 0 then Some start else find_substring text pattern (start + 1)

let starts_with ~prefix text =
  let prefix_len = String.length prefix in
  let text_len = String.length text in
  prefix_len <= text_len && String.sub text 0 prefix_len = prefix

let ends_with ~suffix text =
  let suffix_len = String.length suffix in
  let text_len = String.length text in
  suffix_len <= text_len && String.sub text (text_len - suffix_len) suffix_len = suffix

let replace_substring text pattern replacement =
  if pattern = "" then text
  else
    let buffer = Buffer.create (String.length text) in
    let pattern_len = String.length pattern in
    let rec loop index =
      if index >= String.length text then ()
      else
        match find_substring text pattern index with
        | None ->
            Buffer.add_substring buffer text index (String.length text - index)
        | Some found ->
            Buffer.add_substring buffer text index (found - index);
            Buffer.add_string buffer replacement;
            loop (found + pattern_len)
    in
    loop 0;
    Buffer.contents buffer

let split_string text delimiter =
  if delimiter = "" then
    List.init (String.length text) (fun i -> String.make 1 text.[i])
  else
    let delimiter_len = String.length delimiter in
    let rec loop acc index =
      match find_substring text delimiter index with
      | None ->
          let rest = String.sub text index (String.length text - index) in
          List.rev (rest :: acc)
      | Some found ->
          let part = String.sub text index (found - index) in
          loop (part :: acc) (found + delimiter_len)
    in
    if String.length text = 0 then [ "" ] else loop [] 0

let type_of_value = function
  | V_null -> "none"
  | V_int _ -> "int"
  | V_float _ -> "float"
  | V_bool _ -> "bool"
  | V_string _ -> "string"
  | V_list _ -> "list"
  | V_set _ -> "set"
  | V_record _ -> "record"
  | V_function _ -> "function"
  | V_native _ -> "native"
  | V_module _ -> "module"
  | V_object object_value -> object_value.kind
  | V_length -> "length"

(* --- indexing -------------------------------------------------------------

   items[0] on lists and strings, user["name"] on records. Sets are excluded on
   purpose: their order is not guaranteed, so an index would mean nothing. *)

let index_out_of_bounds kind position length =
  raise
    (Runtime_error
       (Printf.sprintf "%s index %d is out of bounds (length %d)" kind position length))

let index_get container key =
  match (container, key) with
  | V_list items, V_int position ->
      let length = List.length !items in
      if position < 0 || position >= length then index_out_of_bounds "List" position length
      else List.nth !items position
  | V_string text, V_int position ->
      let length = String.length text in
      if position < 0 || position >= length then index_out_of_bounds "String" position length
      else V_string (String.make 1 text.[position])
  | V_record fields, V_string name -> (
      match record_get fields name with
      | Some value -> value
      | None -> raise (Runtime_error (Printf.sprintf "Record has no field '%s'" name)))
  | V_record _, other ->
      raise (Runtime_error ("Record keys must be strings, got " ^ type_of_value other))
  | (V_list _ | V_string _), other ->
      raise (Runtime_error ("Index must be an int, got " ^ type_of_value other))
  | V_set _, _ -> raise (Runtime_error "Sets are unordered and cannot be indexed")
  | other, _ -> raise (Runtime_error ("Cannot index a " ^ type_of_value other))

let index_set container key value =
  match (container, key) with
  | V_list items, V_int position ->
      let length = List.length !items in
      if position < 0 || position >= length then index_out_of_bounds "List" position length
      else items := List.mapi (fun i old -> if i = position then value else old) !items
  | V_record fields, V_string name -> record_set fields name value
  | V_record _, other ->
      raise (Runtime_error ("Record keys must be strings, got " ^ type_of_value other))
  | V_list _, other ->
      raise (Runtime_error ("Index must be an int, got " ^ type_of_value other))
  | V_string _, _ ->
      raise (Runtime_error "Strings are immutable; build a new one instead")
  | V_set _, _ -> raise (Runtime_error "Sets are unordered and cannot be indexed")
  | other, _ -> raise (Runtime_error ("Cannot assign into a " ^ type_of_value other))

let length_of env name =
  match env_lookup env name with
  | V_string s -> V_int (String.length s)
  | V_list items -> V_int (List.length !(items))
  | V_set items -> V_int (List.length !(items))
  | V_record fields -> V_int (List.length !fields)
  | _ -> raise (Runtime_error (Printf.sprintf "Length.%s is not countable" name))

let builtin_print args =
  let output = args |> List.map value_to_string |> String.concat " " in
  print_endline output;
  V_null

let builtin_input args =
  (match args with
  | [] -> ()
  | [ prompt ] ->
      print_string (value_to_string prompt);
      flush stdout
  | _ -> raise (Runtime_error "input expects At most one argument"));
  V_string (read_line ())

let builtin_push args =
  match args with
  | [ target; value ] ->
      let items = expect_list "push" target in
      items := !(items) @ [ value ];
      target
  | _ -> raise (Runtime_error "push expects (list, value)")

let builtin_pull args =
  match args with
  | [ target ] ->
      let items = expect_list "pull" target in
      (match List.rev !(items) with
      | [] -> raise (Runtime_error "pull on empty list")
      | head :: tail_rev ->
          items := List.rev tail_rev;
          head)
  | _ -> raise (Runtime_error "pull expects (list)")

let builtin_insert args =
  match args with
  | [ target; index; value ] ->
      let items = expect_list "insert" target in
      let idx = expect_int index in
      let before, after =
        let rec split i acc rest =
          match (i, rest) with
          | _, [] -> (List.rev acc, [])
          | 0, _ -> (List.rev acc, rest)
          | n, hd :: tl -> split (n - 1) (hd :: acc) tl
        in
        split idx [] !(items)
      in
      items := before @ (value :: after);
      target
  | _ -> raise (Runtime_error "insert expects (list, index, value)")

let builtin_remove args =
  match args with
  | [ target; index ] ->
      let items = expect_list "remove" target in
      let idx = expect_int index in
      let rec remove i acc = function
        | [] -> raise (Runtime_error "remove index out of bounds")
        | hd :: tl ->
            if i = 0 then List.rev acc @ tl else remove (i - 1) (hd :: acc) tl
      in
      items := remove idx [] !(items);
      target
  | _ -> raise (Runtime_error "remove expects (list, index)")

let with_file_error operation path action =
  try action () with
  | Sys_error message ->
      raise
        (Runtime_error
           (Printf.sprintf "%s failed for '%s': %s" operation path message))

let builtin_read_file args =
  match args with
  | [ path_value ] ->
      let path = expect_string "read_file" path_value in
      with_file_error "read_file" path (fun () ->
          let channel = open_in_bin path in
          Fun.protect
            ~finally:(fun () -> close_in channel)
            (fun () -> V_string (really_input_string channel (in_channel_length channel))))
  | _ -> raise (Runtime_error "read_file expects (path)")

let builtin_write_file args =
  match args with
  | [ path_value; content ] ->
      let path = expect_string "write_file" path_value in
      with_file_error "write_file" path (fun () ->
          let channel = open_out_bin path in
          Fun.protect
            ~finally:(fun () -> close_out channel)
            (fun () -> output_string channel (value_to_string content));
          V_null)
  | _ -> raise (Runtime_error "write_file expects (path, content)")

let builtin_file_exists args =
  match args with
  | [ path_value ] ->
      let path = expect_string "file_exists" path_value in
      V_bool (Sys.file_exists path)
  | _ -> raise (Runtime_error "file_exists expects (path)")

let utf8_safe_prefix text limit =
  let stop = min (String.length text) (max 0 limit) in
  let rec find_boundary index =
    if index <= 0 || index >= String.length text then index
    else
      let byte = Char.code text.[index] in
      if byte land 0xC0 = 0x80 then find_boundary (index - 1) else index
  in
  String.sub text 0 (find_boundary stop)

let builtin_truncate args =
  match args with
  | [ text_value; max_value ] ->
      let text = expect_string "truncate" text_value in
      let max_length = expect_int max_value in
      if max_length < 0 then raise (Runtime_error "truncate length cannot be negative");
      if String.length text <= max_length then V_string text
      else if max_length <= 3 then V_string (utf8_safe_prefix text max_length)
      else V_string (utf8_safe_prefix text (max_length - 3) ^ "...")
  | _ -> raise (Runtime_error "truncate expects (text, max_length)")

let create_global_environment () =
  let env = empty_env () in
  env_define env "print" (V_native builtin_print);
  env_define env "input" (V_native builtin_input);
  env_define env "sqrt"
    (V_native (function
      | [ value ] ->
          let (num, _) = expect_number value in
          V_float (sqrt num)
      | _ -> raise (Runtime_error "sqrt expects one argument")));
  env_define env "Pow"
    (V_native (function
      | [ a; b ] -> pow_operator a b
      | _ -> raise (Runtime_error "Pow expects two arguments")));
  env_define env "abs"
    (V_native (function
      | [ v ] -> (
          match v with
          | V_int n -> V_int (abs n)
          | V_float f -> V_float (abs_float f)
          | _ -> raise (Runtime_error "abs expects a number"))
      | _ -> raise (Runtime_error "abs expects one argument")));
  let fold_min_max name args selector =
    match args with
    | [] -> raise (Runtime_error (name ^ " expects at least one argument"))
    | first :: rest ->
        List.fold_left
          (fun acc value ->
            let cmp = compare_values value acc in
            if selector cmp then value else acc)
          first rest
  in
  env_define env "min"
    (V_native (fun args ->
         let result = fold_min_max "min" args (fun cmp -> cmp < 0) in
         result));
  env_define env "max"
    (V_native (fun args ->
         let result = fold_min_max "max" args (fun cmp -> cmp > 0) in
         result));
  env_define env "round"
    (V_native (function
      | [ value ] -> (
          match value with
          | V_int _ as v -> v
          | V_float f ->
              if Float.is_nan f || Float.is_infinite f then
                raise (Runtime_error "round cannot handle NaN or infinity")
              else
                let rounded =
                  if f >= 0.0 then Float.floor (f +. 0.5) else Float.ceil (f -. 0.5)
                in
                V_int (int_of_float rounded)
          | _ -> raise (Runtime_error "round expects a numeric value"))
      | _ -> raise (Runtime_error "round expects one argument")));
  env_define env "type"
    (V_native (function
      | [ value ] -> V_string (type_of_value value)
      | _ -> raise (Runtime_error "type expects one argument")));
  env_define env "range"
    (V_native (function
      | [ start_v; stop_v ] ->
          let start_i = expect_int start_v in
          let stop_i = expect_int stop_v in
          let step = if stop_i >= start_i then 1 else -1 in
          let rec build acc current =
            if (step > 0 && current >= stop_i) || (step < 0 && current <= stop_i) then List.rev acc
            else build (current :: acc) (current + step)
          in
          V_list (ref (build [] start_i |> List.map (fun n -> V_int n)))
      | [ start_v; stop_v; step_v ] ->
          let start_i = expect_int start_v in
          let stop_i = expect_int stop_v in
          let step_i = expect_int step_v in
          if step_i = 0 then raise (Runtime_error "range step cannot be zero");
          let forward = step_i > 0 in
          let rec build acc current =
            if (forward && current >= stop_i) || ((not forward) && current <= stop_i) then List.rev acc
            else build (current :: acc) (current + step_i)
          in
          V_list (ref (build [] start_i |> List.map (fun n -> V_int n)))
      | _ -> raise (Runtime_error "range expects two or three integer arguments")));
  env_define env "assert"
    (V_native (function
      | [ condition ] ->
          if truthy condition then V_null else raise (Runtime_error "Assertion failed")
      | _ -> raise (Runtime_error "assert expects one argument")));
  env_define env "push" (V_native builtin_push);
  env_define env "pull" (V_native builtin_pull);
  env_define env "insert" (V_native builtin_insert);
  env_define env "remove" (V_native builtin_remove);
  env_define env "read_file" (V_native builtin_read_file);
  env_define env "write_file" (V_native builtin_write_file);
  env_define env "file_exists" (V_native builtin_file_exists);
  env_define env "truncate" (V_native builtin_truncate);
  (* len(x) works on any value with a size, and unlike Length.name it takes an
     expression: len(build_list()) is legal, Length.(...) never was. *)
  env_define env "len"
    (V_native (function
      | [ V_string text ] -> V_int (String.length text)
      | [ V_list items ] -> V_int (List.length !items)
      | [ V_set items ] -> V_int (List.length !items)
      | [ V_record fields ] -> V_int (List.length !fields)
      | [ other ] ->
          raise (Runtime_error ("len() does not apply to a " ^ type_of_value other))
      | _ -> raise (Runtime_error "len expects one argument")));
  (* Kept so 0.4 sources still run; len() is the documented form. *)
  env_define env "Length" V_length;
  env
