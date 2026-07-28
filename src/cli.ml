open Parser
open Interpreter
open Runtime

(* Install the Bogue GUI backend into Interpreter's hooks. Done at module
   initialisation rather than inside [main] so that every entry point is
   covered, including run_source/run_file called from generated OCaml stubs. *)
let () = Gui_backend.register ()

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)

let rec find_file_upwards start filename =
  let candidate = Filename.concat start filename in
  if Sys.file_exists candidate then Some start
  else
    let parent = Filename.dirname start in
    if String.equal parent start then None else find_file_upwards parent filename

type update_args = {
  root : string option;
  install_args : string list;
  run_deps : bool;
}

let parse_update_args raw_args =
  let rec aux acc args =
    match args with
    | [] -> acc
    | "--skip-deps" :: rest -> aux { acc with run_deps = false } rest
    | "--deps" :: rest -> aux { acc with run_deps = true } rest
    | "--root" :: value :: rest -> aux { acc with root = Some value } rest
    | opt :: rest when String.length opt >= 7 && String.sub opt 0 7 = "--root=" ->
        let value = String.sub opt 7 (String.length opt - 7) in
        aux { acc with root = Some value } rest
    | arg :: rest ->
        aux { acc with install_args = arg :: acc.install_args } rest
  in
  let initial = { root = None; install_args = []; run_deps = true } in
  let result = aux initial raw_args in
  { result with install_args = List.rev result.install_args }

let resolve_update_root explicit_root =
  let validate_root path =
    let script = Filename.concat path "install.sh" in
    if Sys.file_exists script then Ok path
    else Error (Printf.sprintf "[suchu] install.sh not found in %s" path)
  in
  match explicit_root with
  | Some path -> validate_root path
  | None -> (
      match Sys.getenv_opt "SUCHU_ROOT" with
      | Some env_root -> validate_root env_root
      | None ->
          let cwd = Sys.getcwd () in
          match find_file_upwards cwd "install.sh" with
          | Some root -> Ok root
          | None ->
              Error
                "[suchu] Unable to locate project root. Set --root PATH or SUCHU_ROOT.")

let run_script path args =
  let command =
    let quoted_args =
      match args with
      | [] -> ""
      | _ ->
          " "
          ^ String.concat " "
              (List.map Filename.quote args)
    in
    "bash " ^ Filename.quote path ^ quoted_args
  in
  match Sys.command command with
  | 0 -> Ok ()
  | status ->
      Error
        (Printf.sprintf "[suchu] Command failed (exit code %d): %s" status command)

let run_update raw_args =
  let args = parse_update_args raw_args in
  match resolve_update_root args.root with
  | Error msg ->
      prerr_endline msg;
      1
  | Ok root ->
      let steps =
        let install_step =
          (Filename.concat root "install.sh", true, args.install_args)
        in
        if args.run_deps then
          [
            (Filename.concat root "install_deps.sh", false, []);
            install_step;
          ]
        else [ install_step ]
      in
      let rec execute = function
        | [] ->
            Printf.printf "[suchu] Update complete.\n%!";
            0
        | (path, required, extra_args) :: rest ->
            if Sys.file_exists path then (
              Printf.printf "[suchu] Running %s...\n%!" path;
              match run_script path extra_args with
              | Ok () -> execute rest
              | Error msg ->
                  prerr_endline msg;
                  1)
            else if required then (
              prerr_endline
                (Printf.sprintf "[suchu] Required script missing: %s" path);
              1)
            else (
              Printf.printf "[suchu] Skipping missing script: %s\n%!" path;
              execute rest)
      in
      execute steps

let run_source ?base_dir ?embedded_modules source =
  let program = parse source in
  let interpreter = create ?base_dir ?embedded_modules () in
  ignore (run_program interpreter program)

let run_file path =
  let source = read_file path in
  let absolute_path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  run_source ~base_dir:(Filename.dirname absolute_path) source

let find_substring text needle start =
  let text_length = String.length text in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then None
    else if String.sub text index needle_length = needle then Some index
    else loop (index + 1)
  in
  loop start

let parse_number text start =
  let rec loop index =
    if index < String.length text && text.[index] >= '0' && text.[index] <= '9' then loop (index + 1)
    else index
  in
  let stop = loop start in
  if stop = start then None else Some (int_of_string (String.sub text start (stop - start)), stop)

let error_location message =
  match find_substring message "line " 0 with
  | None -> None
  | Some line_start -> (
      match parse_number message (line_start + 5) with
      | None -> None
      | Some (line, after_line) -> (
          match find_substring message "column " after_line with
          | None -> None
          | Some column_start ->
              Option.map (fun (column, _) -> (line, column)) (parse_number message (column_start + 7))))

let show_source_excerpt source message =
  match error_location message with
  | None -> ()
  | Some (line_number, column) ->
      let lines = String.split_on_char '\n' source in
      (match List.nth_opt lines (line_number - 1) with
      | None -> ()
      | Some line ->
          Printf.eprintf "  %d | %s\n" line_number line;
          Printf.eprintf "    | %s^\n" (String.make (max 0 (column - 1)) ' '))

let report_source_error kind source message =
  Printf.eprintf "%s: %s\n" kind message;
  show_source_excerpt source message

(* The CLI entry points hold a path, but show_source_excerpt needs the text to
   quote the offending line. Re-read it, and fall back to no excerpt if the file
   is unreadable -- a diagnostic must never raise over the error it reports. *)
let source_text_for_diagnostics path = try read_file path with _ -> ""

let report_file_error kind path message =
  report_source_error kind (source_text_for_diagnostics path) message

let repl () =
  let interpreter = ref (create ()) in
  print_endline "Suchu REPL 0.5 - enter :help for commands.";
  let rec loop () =
    print_string "suchu> ";
    flush stdout;
    match read_line () with
    | exception End_of_file -> print_endline "\nBye."
    | input ->
        let input = String.trim input in
        (match input with
        | "" -> ()
        | ":quit" | ":exit" -> raise Exit
        | ":help" ->
            print_endline "Enter one Suchu expression or statement per line.";
            print_endline "Commands: :help, :reset, :quit (or :exit)."
        | ":reset" ->
            interpreter := create ();
            print_endline "The REPL environment has been reset.";
        | source ->
            (* Statements need a ';' since 0.5, but typing one at every REPL
               prompt is tedious, so add it when the line clearly lacks one.
               A line ending in '}' closes a block and needs nothing. *)
            let source =
              let last = source.[String.length source - 1] in
              if last = ';' || last = '}' then source else source ^ ";"
            in
            (try
               let value = run_program !interpreter (parse source) in
               print_endline (value_to_string value)
             with
            | Lexical.Lexing_error message -> report_source_error "Lexing error" source message
            | Parser.Parse_error message -> report_source_error "Parse error" source message
            | Runtime_error message -> Printf.eprintf "Runtime error: %s\n" message));
        loop ()
  in
  try loop () with Exit -> print_endline "Bye."

let generate_stub source =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (fun ch ->
      match ch with
      | '\\' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer '\\'
      | '"' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer '"'
      | '\n' ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer 'n'
      | _ -> Buffer.add_char buffer ch)
    source;
  let escaped = Buffer.contents buffer in
  Printf.sprintf
    "(* Auto-generated Suchu 0.5 program *)\nlet suchu_source = \"%s\"\n\nlet () =\n  \
     Suchu.Cli.run_source suchu_source\n"
    escaped

let compile_to_stub source_path output_path =
  let source = read_file source_path in
  ignore (parse source); (* ensure the program is syntactically valid *)
  let stub = generate_stub source in
  write_file output_path stub

let rec imports_in_statements statements =
  List.concat_map
    (function
      | Ast.Import module_spec -> [ module_spec ]
      | Ast.FunctionDef declaration -> imports_in_statements declaration.body
      | Ast.IfStmt (_, then_block, else_block) ->
          imports_in_statements then_block
          @ Option.fold ~none:[] ~some:imports_in_statements else_block
      | Ast.MatchStmt (_, branches, default_block) ->
          List.concat_map (fun (_, block) -> imports_in_statements block) branches
          @ Option.fold ~none:[] ~some:imports_in_statements default_block
      | Ast.WhileStmt (_, block)
      | Ast.ForStmt (_, _, block) -> imports_in_statements block
      (* Both arms must be walked, or an import inside a try would be missing
         from a bundled binary. *)
      | Ast.TryStmt (attempted, _, handler) ->
          imports_in_statements attempted @ imports_in_statements handler
      | Ast.Gui _
      | Ast.ExprStmt _
      | Ast.Assign _
      | Ast.Break
      | Ast.Continue
      | Ast.Return _ -> [])
    statements

let bundle_modules source_path source =
  let source_absolute =
    if Filename.is_relative source_path then Filename.concat (Sys.getcwd ()) source_path
    else source_path
  in
  let visited = Hashtbl.create 16 in
  let rec collect real_dir virtual_dir program =
    imports_in_statements program
    |> List.concat_map (fun module_spec ->
           match Runtime.find_native_module module_spec with
           | Some _ -> []
           | None ->
               let real_path = Interpreter.resolve_module_path real_dir module_spec in
               let virtual_path = Interpreter.resolve_module_path virtual_dir module_spec in
               if Hashtbl.mem visited real_path then []
               else if not (Sys.file_exists real_path) then
                 raise
                   (Runtime_error
                      (Printf.sprintf "Cannot bundle import '%s': file not found at %s"
                         module_spec real_path))
               else
                 let module_source = read_file real_path in
                 let module_program =
                   try parse module_source with
                   | Lexical.Lexing_error message ->
                       raise
                         (Runtime_error
                            (Printf.sprintf "In imported module '%s': %s" module_spec message))
                   | Parser.Parse_error message ->
                       raise
                         (Runtime_error
                            (Printf.sprintf "In imported module '%s': %s" module_spec message))
                 in
                 Hashtbl.add visited real_path ();
                 (virtual_path, module_source)
                 :: collect (Filename.dirname real_path) (Filename.dirname virtual_path)
                      module_program)
  in
  collect (Filename.dirname source_absolute) "/suchu-bundle" (parse source)

let sanitize_filename name =
  String.map
    (fun ch ->
      match ch with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | '-' -> ch
      | _ -> '_')
    name

let ensure_directory path =
  if Sys.file_exists path then (
    if not (Sys.is_directory path) then
      raise (Runtime_error (Printf.sprintf "%s exists and is not a directory" path)))
  else Sys.mkdir path 0o755

let generate_native_entry source bundled_modules =
  let modules =
    bundled_modules
    |> List.map (fun (path, module_source) -> Printf.sprintf "    (%S, %S);" path module_source)
    |> String.concat "\n"
  in
  Printf.sprintf
    "let source = %S\n\nlet embedded_modules = [\n%s\n  ]\n\nlet () =\n  \
     Suchu.Cli.run_source ~base_dir:%S ~embedded_modules source\n"
    source modules "/suchu-bundle"

let build_native source_path output_path =
  Suchu_stdlib.register ();
  let source = read_file source_path in
  ignore (parse source);
  let bundled_modules = bundle_modules source_path source in
  let project_root =
    match find_file_upwards (Sys.getcwd ()) "dune-project" with
    | Some root -> root
    | None ->
        raise
          (Runtime_error
             "suchu build currently needs to run inside the Suchu project checkout")
  in
  let source_name =
    Filename.basename source_path
    |> Filename.remove_extension
    |> sanitize_filename
  in
  let build_root = Filename.concat project_root "build" in
  ensure_directory build_root;
  let app_dir =
    Filename.concat build_root
      (source_name ^ "-" ^ String.sub (Digest.to_hex (Digest.string source_path)) 0 8)
  in
  ensure_directory app_dir;
  write_file (Filename.concat app_dir "main.ml")
    (generate_native_entry source bundled_modules);
  write_file (Filename.concat app_dir "dune")
    "(executable\n (name main)\n (libraries suchu))\n";
  let relative_app_dir =
    Filename.concat "build" (Filename.basename app_dir)
  in
  let target = Filename.concat relative_app_dir "main.exe" in
  let build_command =
    Printf.sprintf "dune build --root %s %s"
      (Filename.quote project_root) (Filename.quote target)
  in
  if Sys.command build_command <> 0 then
    raise (Runtime_error "Native compilation failed");
  let built_binary =
    Filename.concat
      (Filename.concat (Filename.concat project_root "_build") "default")
      target
  in
  let copy_command =
    Printf.sprintf "cp %s %s" (Filename.quote built_binary) (Filename.quote output_path)
  in
  if Sys.command copy_command <> 0 then
    raise
      (Runtime_error
         (Printf.sprintf "Could not copy native executable to %s" output_path));
  Printf.printf "[suchu] Native executable: %s\n" output_path;
  Printf.printf "[suchu] Bundled modules: %d\n" (List.length bundled_modules);
  Printf.printf "[suchu] Target: Linux/WSL (%s)\n%!" Sys.os_type

let usage () =
  print_endline "Usage:";
  print_endline "  suchu run <file.suchu>";
  print_endline "  suchu comp <file.suchu> <output.ml>";
  print_endline "  suchu build <file.suchu> [-o output]";
  print_endline "  suchu repl";
  print_endline "  suchu update [--root PATH] [--skip-deps] [install.sh options]"

let main argv =
  match Array.to_list argv with
  | _ :: "run" :: source :: [] -> (
      try
        run_file source;
        0
      with
      | Runtime_error msg ->
          prerr_endline ("Runtime error: " ^ msg);
          3
      | Lexical.Lexing_error msg ->
          report_file_error "Lexing error" source msg;
          2
      | Parser.Parse_error msg ->
          report_file_error "Parse error" source msg;
          2)
  | _ :: "comp" :: source :: output :: [] -> (
      try
        compile_to_stub source output;
        Printf.printf "[suchu] OCaml stub generated At %s\n" output;
        0
      with
      | Runtime_error msg ->
          prerr_endline ("Runtime error: " ^ msg);
          3
      | Lexical.Lexing_error msg ->
          report_file_error "Lexing error" source msg;
          2
      | Parser.Parse_error msg ->
          report_file_error "Parse error" source msg;
          2)
  | _ :: "build" :: source :: rest -> (
      let output =
        match rest with
        | [] -> Filename.remove_extension source
        | [ "-o"; path ]
        | [ "--output"; path ] -> path
        | _ ->
            prerr_endline "Usage: suchu build <file.suchu> [-o output]";
            ""
      in
      if String.equal output "" then 1
      else
        try
          build_native source output;
          0
        with
        | Runtime_error msg ->
            prerr_endline ("Build error: " ^ msg);
            3
        | Lexical.Lexing_error msg ->
            report_file_error "Lexing error" source msg;
            2
        | Parser.Parse_error msg ->
            report_file_error "Parse error" source msg;
            2)
  | _ :: "repl" :: [] ->
      repl ();
      0
  | _ :: "update" :: rest -> run_update rest
  | _ ->
      usage ();
      1
