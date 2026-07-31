(* Interpreted against compiled, byte for byte.

   Suchu now has two ways of running a program, and nothing stops them drifting
   apart except that they are checked against each other. The compiler emits
   calls to the very same operator functions the evaluator calls, which removes
   most of the ways they could disagree -- but not the ways that live in the
   translation itself: an operand evaluated twice, a break caught by the wrong
   loop, a variable that outlives its block, a unary minus that loses the sign
   of zero.

   Those are the mistakes this catches. Each program in tests/differential is
   run both ways and the two outputs are compared as bytes, not as values, so a
   float that prints differently counts as a failure.

   Why this exists now rather than later: the next step is to stop boxing
   integers, and an unboxing mistake does not raise, it computes a wrong
   answer. From here on the interpreter is the specification and this is how the
   compiler is held to it.

   It is an executable and not a dune test on purpose. 'suchu comp' runs
   'dune build', and a dune test cannot do that -- the outer dune already holds
   the lock on the build directory. So it is run by hand, after a build:

     dune build
     ./_build/default/tools/differential.exe *)

(* tests/differential holds programs written to corner the language. examples
   holds the ones written to be read, which is a different and useful kind of
   check: they were not designed with the compiler in mind.

   Only the top level of examples, not examples/gui -- a window waits for a human
   and would hang the suite. *)
let suite_directories = [ Filename.concat "tests" "differential"; "examples" ]

(* --- shelling out --------------------------------------------------------- *)

let read_file path =
  let channel = open_in_bin path in
  let length = in_channel_length channel in
  let contents = really_input_string channel length in
  close_in channel;
  contents

let scratch = Filename.concat (Filename.get_temp_dir_name ()) "suchu-differential"

let temp name = Filename.concat scratch name

(* Both streams are kept, and separately: a program that prints the right
   answer to the wrong stream is not the same program.

   The child is started directly rather than through a shell. Going through one
   would mean quoting a path for whichever shell happens to be there, and cmd.exe
   and sh disagree about that; redirecting onto file descriptors sidesteps the
   question on both. *)
let run program arguments =
  let out_path = temp "stdout" and err_path = temp "stderr" in
  let create path = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  let out = create out_path and err = create err_path in
  let pid =
    Unix.create_process program (Array.of_list (program :: arguments)) Unix.stdin out err
  in
  let _, status = Unix.waitpid [] pid in
  Unix.close out;
  Unix.close err;
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal
  in
  (code, read_file out_path, read_file err_path)

(* --- reporting ------------------------------------------------------------ *)

let lines text = String.split_on_char '\n' text

(* A whole-file dump would bury the one line that matters, so the report names
   the first place the two disagree and shows only that. *)
let first_difference expected actual =
  let rec walk index expected actual =
    match (expected, actual) with
    | [], [] -> None
    | e :: _, [] -> Some (index, e, "<end of output>")
    | [], a :: _ -> Some (index, "<end of output>", a)
    | e :: expected_rest, a :: actual_rest ->
        if String.equal e a then walk (index + 1) expected_rest actual_rest
        else Some (index, e, a)
  in
  walk 1 (lines expected) (lines actual)

let passed = ref 0
let failed = ref 0
let skipped = ref 0

let report_mismatch program stream expected actual =
  incr failed;
  Printf.printf "FAIL  %s\n" program;
  Printf.printf "      the two disagree on %s\n" stream;
  (match first_difference expected actual with
  | Some (line, expected_line, actual_line) ->
      Printf.printf "      line %d\n" line;
      Printf.printf "        interpreted: %s\n" expected_line;
      Printf.printf "        compiled:    %s\n" actual_line
  | None -> Printf.printf "      (identical line by line; the difference is in trailing bytes)\n");
  print_newline ()

(* --- one program ---------------------------------------------------------- *)

let check suchu (source, program) =
  (* Windows will not start a file without the extension, so the name has to
     carry it there and must not elsewhere. *)
  let binary = temp (if Sys.win32 then "compiled.exe" else "compiled") in
  let interpreted_status, interpreted_out, interpreted_err = run suchu [ "run"; source ] in
  let compile_status, _, compile_err = run suchu [ "comp"; source; "-o"; binary ] in
  if compile_status <> 0 then
    (* A program the compiler openly declines is not a failure of this suite --
       the coverage gap is already known. A program it fails to compile for any
       other reason is. *)
    let declined =
      let needle = "does not handle" in
      let rec contains index =
        if index + String.length needle > String.length compile_err then false
        else if String.sub compile_err index (String.length needle) = needle then true
        else contains (index + 1)
      in
      contains 0
    in
    if declined then begin
      incr skipped;
      Printf.printf "SKIP  %s\n" program;
      Printf.printf "      outside the compiler's coverage:\n";
      List.iter
        (fun line ->
          let line = String.trim line in
          if line <> "" then Printf.printf "      %s\n" line)
        (lines compile_err);
      print_newline ()
    end
    else begin
      incr failed;
      Printf.printf "FAIL  %s\n" program;
      Printf.printf "      compilation failed:\n%s\n\n" compile_err
    end
  else
    let native_status, native_out, native_err = run binary [] in
    if not (String.equal interpreted_out native_out) then
      report_mismatch program "standard output" interpreted_out native_out
    else if not (String.equal interpreted_err native_err) then
      report_mismatch program "standard error" interpreted_err native_err
    else if interpreted_status <> native_status then begin
      incr failed;
      Printf.printf "FAIL  %s\n" program;
      Printf.printf "      same output, different exit status: interpreted %d, compiled %d\n\n"
        interpreted_status native_status
    end
    else begin
      incr passed;
      Printf.printf "ok    %s (%d lines)\n" program (List.length (lines interpreted_out) - 1)
    end

(* --- the suite ------------------------------------------------------------ *)

let () =
  let suchu =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else Filename.concat "_build" (Filename.concat "default" (Filename.concat "src" "suchu_cli.exe"))
  in
  if not (Sys.file_exists suchu) then begin
    Printf.eprintf "No Suchu binary at %s. Run 'dune build' first, or pass the path.\n" suchu;
    exit 2
  end;
  List.iter
    (fun directory ->
      if not (Sys.file_exists directory) then begin
        Printf.eprintf "Run this from the project root: %s does not exist here.\n" directory;
        exit 2
      end)
    suite_directories;
  if not (Sys.file_exists scratch) then Sys.mkdir scratch 0o755;
  let programs =
    suite_directories
    |> List.concat_map (fun directory ->
           Sys.readdir directory |> Array.to_list
           |> List.filter (fun name -> Filename.check_suffix name ".suchu")
           |> List.sort String.compare
           |> List.map (fun name -> (Filename.concat directory name, Filename.concat directory name)))
  in
  Printf.printf "Interpreted against compiled, %d programs.\n\n" (List.length programs);
  List.iter (check suchu) programs;
  Printf.printf "\n%d identical, %d differing, %d outside coverage.\n" !passed !failed !skipped;
  if !failed > 0 then exit 1
