let fail name message =
  Printf.eprintf "[FAIL] %s: %s\n%!" name message;
  exit 1

let pass name = Printf.printf "[PASS] %s\n%!" name

let check name condition message =
  if condition then pass name else fail name message

let contains text needle =
  let rec loop index =
    if index + String.length needle > String.length text then false
    else if String.sub text index (String.length needle) = needle then true
    else loop (index + 1)
  in
  loop 0

let eval ?base_dir source =
  let interpreter = Interpreter.create ?base_dir () in
  Interpreter.run_program interpreter (Parser.parse source)

(* Shipped broken until 0.7: both operands were evaluated before the operator
   was consulted, so the commonest guard in programming -- check a length, then
   index -- raised instead of short-circuiting. *)
let test_short_circuit () =
  let guarded = "xs = []; if len(xs) > 0 && xs[0] == 1 { 1; } else { 2; }" in
  (match eval guarded with
  | Runtime.V_int 2 -> pass "&& short-circuits"
  | value -> fail "&& short-circuits" ("expected 2, got " ^ Runtime.value_to_string value));
  (* The right-hand side must not run at all, not merely be ignored. *)
  let counted = "n = 0; fun bump() { n = n + 1; return true; } false && bump(); true || bump(); n;" in
  match eval counted with
  | Runtime.V_int 0 -> pass "&& and || leave the right side alone"
  | value ->
      fail "&& and || leave the right side alone"
        ("the right-hand side ran; n = " ^ Runtime.value_to_string value)

(* 2 ** 3 used to be 8.0: the operator always went through floats. *)
let test_integer_power () =
  (match eval "2 ** 3;" with
  | Runtime.V_int 8 -> pass "integer power stays integer"
  | value -> fail "integer power stays integer" ("expected 8, got " ^ Runtime.value_to_string value));
  (match eval "2 ** 40;" with
  | Runtime.V_int 1099511627776 -> pass "integer power is exact"
  | value -> fail "integer power is exact" ("expected 1099511627776, got " ^ Runtime.value_to_string value));
  (* A negative exponent and an overflowing result both have to leave the integers. *)
  (match eval "2 ** (0 - 1);" with
  | Runtime.V_float f when Float.abs (f -. 0.5) < 1e-12 -> pass "negative exponent gives a float"
  | value -> fail "negative exponent gives a float" ("expected 0.5, got " ^ Runtime.value_to_string value));
  match eval "2 ** 200;" with
  | Runtime.V_float _ -> pass "overflowing power gives a float"
  | value -> fail "overflowing power gives a float" ("expected a float, got " ^ Runtime.value_to_string value)

let test_arithmetic () =
  match eval "2 + 3 * 4;" with
  | Runtime.V_int 14 -> pass "arithmetic"
  | value -> fail "arithmetic" ("expected 14, got " ^ Runtime.value_to_string value)

let test_closures () =
  let source =
    {|
fun make_adder(value) {
  return fun(other) { return value + other; };
}
add_five = make_adder(5);
add_five(7);
|}
  in
  match eval source with
  | Runtime.V_int 12 -> pass "closures"
  | value -> fail "closures" ("expected 12, got " ^ Runtime.value_to_string value)

let test_collections () =
  let source =
    {|
values = [3, 1, 2];
values.sort();
push(values, 4);
values.sum();
|}
  in
  match eval source with
  | Runtime.V_int 10 -> pass "collections"
  | value -> fail "collections" ("expected 10, got " ^ Runtime.value_to_string value)

let test_parse_location () =
  try
    ignore (Parser.parse "value = 1 +");
    fail "parse location" "invalid source was accepted"
  with
  | Parser.Parse_error message ->
      check "parse location" (contains message "line 1" && contains message "column 12")
        ("missing location in: " ^ message)

let test_file_helpers () =
  let path = Filename.temp_file "suchu-test-" ".txt" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let source =
        Printf.sprintf
          "write_file(%S, \"hello\");\nread_file(%S);"
          path path
      in
      match eval source with
      | Runtime.V_string "hello" -> pass "file helpers"
      | value -> fail "file helpers" ("unexpected value: " ^ Runtime.value_to_string value))

let test_truncate_utf8 () =
  match eval {|truncate("éééééééé", 9);|} with
  | Runtime.V_string text ->
      check "UTF-8 truncate" (String.equal text "ééé...")
        ("unexpected truncation: " ^ text)
  | value -> fail "UTF-8 truncate" ("unexpected value: " ^ Runtime.value_to_string value)

let with_temp_modules action =
  let marker = Filename.temp_file "suchu-modules-" ".tmp" in
  let directory = Filename.dirname marker in
  let prefix = Filename.basename marker |> Filename.chop_extension in
  Sys.remove marker;
  let path name = Filename.concat directory (prefix ^ "-" ^ name ^ ".suchu") in
  let write path contents =
    let channel = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel contents)
  in
  let created = ref [] in
  let create name contents =
    let module_path = path name in
    write module_path contents;
    created := module_path :: !created;
    module_path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun file -> if Sys.file_exists file then Sys.remove file) !created)
    (fun () -> action directory prefix create)

let test_import_namespace () =
  with_temp_modules (fun directory prefix create ->
      ignore (create "math" "answer = 40 + 2;\nfun twice(value) { return value * 2; }");
      let module_name = prefix ^ "-math.suchu" in
      let alias = Runtime.module_alias module_name in
      let source =
        Printf.sprintf "import %S;\n%s.twice(%s.answer);"
          module_name alias alias
      in
      match eval ~base_dir:directory source with
      | Runtime.V_int 84 -> pass "import namespace"
      | value ->
          fail "import namespace"
            ("expected 84, got " ^ Runtime.value_to_string value))

let test_nested_import () =
  with_temp_modules (fun directory prefix create ->
      ignore (create "base" "value = 21;");
      ignore
        (create "feature"
           (let module_name = prefix ^ "-base.suchu" in
            Printf.sprintf "import %S;\nresult = %s.value * 2;"
              module_name (Runtime.module_alias module_name)));
      let module_name = prefix ^ "-feature.suchu" in
      let source =
        Printf.sprintf "import %S;\n%s.result;"
          module_name (Runtime.module_alias module_name)
      in
      match eval ~base_dir:directory source with
      | Runtime.V_int 42 -> pass "nested import"
      | value ->
          fail "nested import"
            ("expected 42, got " ^ Runtime.value_to_string value))

let test_circular_import () =
  with_temp_modules (fun directory prefix create ->
      ignore
        (create "a"
           (Printf.sprintf "import %S;\nvalue = 1;" (prefix ^ "-b.suchu")));
      ignore
        (create "b"
           (Printf.sprintf "import %S;\nvalue = 2;" (prefix ^ "-a.suchu")));
      try
        ignore
          (eval ~base_dir:directory
             (Printf.sprintf "import %S;" (prefix ^ "-a.suchu")));
        fail "circular import" "cycle was accepted"
      with
      | Runtime.Runtime_error message ->
          check "circular import" (contains message "Circular import")
            ("unexpected error: " ^ message))

let test_native_module () =
  Native_api.register_module "test_native"
    [
      Native_api.function2 "add" (fun left right ->
          Native_api.int (Native_api.as_int left + Native_api.as_int right));
      ("name", Native_api.string "OCaml");
    ];
  match eval "import test_native;\ntest_native.add(20, 22);" with
  | Runtime.V_int 42 -> pass "native module API"
  | value ->
      fail "native module API"
        ("expected 42, got " ^ Runtime.value_to_string value)

let test_standard_modules () =
  match
    eval
      "import json;\nimport system;\nencoded = json.encode([1, true, \"ok\"]);\n\
       decoded = json.decode(encoded);\n[encoded, decoded, system.os];"
  with
  | Runtime.V_list values -> (
      match Runtime.list_values values with
      | [ Runtime.V_string "[1,true,\"ok\"]"; Runtime.V_list decoded; Runtime.V_string os ] ->
          check "standard modules"
            (Runtime.list_length decoded = 3 && not (String.equal os ""))
            "unexpected decoded values or platform"
      | _ -> fail "standard modules" "unexpected module result")
  | value ->
      fail "standard modules"
        ("unexpected value: " ^ Runtime.value_to_string value)

(* Found by the differential suite on its first run: a function with no return
   handed back the value of its last statement, so 'fun f(n) { x = n * 2; }'
   answered with x. The documentation has always said none. *)
let test_falling_off_the_end () =
  (match eval "fun f(n) { x = n * 2; } f(4);" with
  | Runtime.V_null -> pass "a function without a return yields none"
  | value ->
      fail "a function without a return yields none"
        ("expected none, got " ^ Runtime.value_to_string value));
  (* The top level still has a value: the REPL prints it. *)
  match eval "1 + 1;" with
  | Runtime.V_int 2 -> pass "the top level still has a value"
  | value ->
      fail "the top level still has a value" ("expected 2, got " ^ Runtime.value_to_string value)

(* Also from the differential suite. Prefix '-' was compiled as 'zero minus',
   which turns -0.0 into 0.0; both sides now negate through one function. *)
let test_negation () =
  (match eval "0 - 7;" with
  | Runtime.V_int (-7) -> pass "integer negation"
  | value -> fail "integer negation" ("expected -7, got " ^ Runtime.value_to_string value));
  match eval "-0.0;" with
  | Runtime.V_float f when Float.sign_bit f -> pass "negative zero keeps its sign"
  | value ->
      fail "negative zero keeps its sign"
        ("expected -0.0, got " ^ Runtime.value_to_string value)

(* Suchu evaluates left to right. OCaml does not promise that for the arguments
   of a function, and in practice goes the other way, so the compiled form used
   to run the operands of every call and every operator backwards. *)
let test_evaluation_order () =
  let source =
    "order = \"\";\n\
     fun note(letter) { order = order @ letter; return 1; }\n\
     fun three(a, b, c) { return 0; }\n\
     three(note(\"a\"), note(\"b\"), note(\"c\"));\n\
     note(\"d\") + note(\"e\");\n\
     order;"
  in
  match eval source with
  | Runtime.V_string "abcde" -> pass "operands are evaluated left to right"
  | value ->
      fail "operands are evaluated left to right"
        ("expected \"abcde\", got " ^ Runtime.value_to_string value)

let () =
  test_arithmetic ();
  test_short_circuit ();
  test_integer_power ();
  test_falling_off_the_end ();
  test_negation ();
  test_evaluation_order ();
  test_closures ();
  test_collections ();
  test_parse_location ();
  test_file_helpers ();
  test_truncate_utf8 ();
  test_import_namespace ();
  test_nested_import ();
  test_circular_import ();
  test_native_module ();
  test_standard_modules ();
  print_endline "All Suchu tests passed."
