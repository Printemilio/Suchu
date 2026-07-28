open Lexical

exception Parse_error of string

type state = {
  tokens : token array;
  mutable index : int;
  length : int;
}

let create_state tokens =
  let arr = Array.of_list tokens in
  { tokens = arr; index = 0; length = Array.length arr }

let peek state =
  if state.index >= state.length then state.tokens.(state.length - 1) else state.tokens.(state.index)

let peek_next state =
  if state.index + 1 >= state.length then state.tokens.(state.length - 1)
  else state.tokens.(state.index + 1)

let previous state =
  state.tokens.(max 0 (state.index - 1))

let advance state =
  if state.index < state.length then state.index <- state.index + 1;
  previous state

let check state predicate =
  if state.index >= state.length then false else predicate state.tokens.(state.index).kind

let match_token state predicate =
  if check state predicate then (
    ignore (advance state);
    true)
  else false

let fail state message =
  let token = peek state in
  raise
    (Parse_error
       (Printf.sprintf "%s (line %d, column %d)" message token.line token.column))

let expect state predicate message =
  if match_token state predicate then previous state else fail state message

let expect_specific state expected message =
  expect state (fun kind -> kind = expected) message

let skip_newlines state =
  while match_token state (function Newline -> true | _ -> false) do
    ()
  done
