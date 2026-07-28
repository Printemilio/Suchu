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
      let alias = Interpreter.module_alias module_name in
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
              module_name (Interpreter.module_alias module_name)));
      let module_name = prefix ^ "-feature.suchu" in
      let source =
        Printf.sprintf "import %S;\n%s.result;"
          module_name (Interpreter.module_alias module_name)
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
      match !values with
      | [ Runtime.V_string "[1,true,\"ok\"]"; Runtime.V_list decoded; Runtime.V_string os ] ->
          check "standard modules"
            (List.length !decoded = 3 && not (String.equal os ""))
            "unexpected decoded values or platform"
      | _ -> fail "standard modules" "unexpected module result")
  | value ->
      fail "standard modules"
        ("unexpected value: " ^ Runtime.value_to_string value)

let () =
  test_arithmetic ();
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
