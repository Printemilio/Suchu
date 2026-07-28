(* JavaScript entry point for the Suchu web playground.

   Compiled with js_of_ocaml against suchu_eval, which carries no GUI
   dependency. This is the real lexer, parser and evaluator — not a
   reimplementation — so the playground cannot drift from the language.

   Exposes one global function:

     SuchuRun(source) -> { output: string, error: string|null, kind: string }

   It is intended to run inside a Web Worker: a Suchu program can loop forever
   and there is no instruction budget in the evaluator, so the host terminates
   the worker on timeout. *)

open Js_of_ocaml

let captured = Buffer.create 4096

(* Route OCaml's stdout/stderr into a buffer instead of the JS console, so that
   print(...) inside a Suchu program is what the playground displays. *)
let () =
  Sys_js.set_channel_flusher stdout (Buffer.add_string captured);
  Sys_js.set_channel_flusher stderr (Buffer.add_string captured)

type outcome = {
  output : string;
  error : string option;
  kind : string;
}

let run_source source =
  Buffer.clear captured;
  let finish error kind =
    (* Flush before reading: print_endline may still be buffered. *)
    (try flush stdout with _ -> ());
    (try flush stderr with _ -> ());
    { output = Buffer.contents captured; error; kind }
  in
  match
    let program = Parser.parse source in
    let interp = Interpreter.create ~base_dir:"/" () in
    ignore (Interpreter.run_program interp program)
  with
  | () -> finish None "ok"
  | exception Lexical.Lexing_error message ->
      finish (Some ("Syntax error: " ^ message)) "lex"
  | exception Parser_utils.Parse_error message ->
      finish (Some ("Parse error: " ^ message)) "parse"
  | exception Runtime.Runtime_error message ->
      finish (Some ("Runtime error: " ^ message)) "runtime"
  | exception Stack_overflow ->
      finish (Some "Runtime error: stack overflow (infinite recursion?)") "runtime"
  | exception End_of_file ->
      (* input() has no stdin in a browser. *)
      finish (Some "Runtime error: input() is not available in the playground") "runtime"
  | exception exn ->
      finish (Some ("Internal error: " ^ Printexc.to_string exn)) "internal"

(* Js.Unsafe.set rather than the ##. syntax, so this module needs no ppx. *)
let () =
  Js.Unsafe.set Js.Unsafe.global (Js.string "SuchuRun")
    (Js.wrap_callback (fun source ->
         let result = run_source (Js.to_string source) in
         Js.Unsafe.obj
           [|
             ("output", Js.Unsafe.inject (Js.string result.output));
             ( "error",
               match result.error with
               | None -> Js.Unsafe.inject Js.null
               | Some message -> Js.Unsafe.inject (Js.string message) );
             ("kind", Js.Unsafe.inject (Js.string result.kind));
           |]))
