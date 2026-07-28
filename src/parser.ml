open Ast
open Lexical
open Parser_utils

type parser_state = Parser_utils.state
exception Parse_error = Parser_utils.Parse_error

(* Field names live in their own namespace, so a reserved word is still a legal
   field: `config.window` and `{ set: 1; }` both work, exactly as `{if: 1}` does
   in JavaScript. Every word token carries its source text in [lexeme], which is
   what makes this a one-liner; quoted keys are accepted too so json.decode can
   round-trip arbitrary names. *)
let field_name_of_token token =
  match token.kind with
  | Ident name -> Some name
  | String_lit name -> Some name
  | _ -> (
      match token.lexeme with
      | Some text when text <> "" && Util.is_alpha text.[0] -> Some text
      | _ -> None)

let starts_expression = function
  | Int_lit _
  | Float_lit _
  | String_lit _
  | Ident _
  | Lparen
  | Lbracket
  | True_kw
  | False_kw
  | None_kw
  | Nan_kw
  | Inf_kw
  | Set_kw
  | Minus
  | Plus
  | Bang
  | At
  | If_kw -> true
  | _ -> false

let rec parse source =
  let tokens = lex source in
  let state = create_state tokens in
  let rec gather acc =
    skip_newlines state;
    if check state (function Eof -> true | _ -> false) then List.rev acc
    else
      let stmt = declaration state in
      gather (stmt :: acc)
  in
  gather []

and declaration state =
  match peek state with
  | { kind = Fun_kw; _ } ->
      let next = peek_next state in
      (match next.kind with
      | Ident _ ->
          ignore (advance state);
          parse_function state
      | _ -> statement state)
  | _ -> statement state

and parse_parameter_list state missing_lparen_message =
  skip_newlines state;
  ignore (expect_specific state Lparen missing_lparen_message);
  let rec parameters acc =
    skip_newlines state;
    match peek state with
    | { kind = Rparen; _ } ->
        ignore (advance state);
        List.rev acc
    | _ ->
        let param =
          match expect state (function Ident _ -> true | _ -> false) "Expected parameter name" with
          | { kind = Ident name; _ } -> name
          | _ -> assert false
        in
        skip_newlines state;
        (match peek state with
        | { kind = Comma; _ } ->
            ignore (advance state);
            parameters (param :: acc)
        | { kind = Rparen; _ } ->
            ignore (advance state);
            List.rev (param :: acc)
        | _ -> fail state "Expected ',' or ')' in parameter list")
  in
  parameters []

and parse_function state =
  let name =
    match expect state (function Ident _ -> true | _ -> false) "Expected function name" with
    | { kind = Ident name; _ } -> name
    | _ -> assert false
  in
  let params = parse_parameter_list state "Expected '(' after function name" in
  skip_newlines state;
  let body = block state in
  FunctionDef { name; params; body }

and parse_function_literal state =
  let params = parse_parameter_list state "Expected '(' after 'fun'" in
  skip_newlines state;
  let body = block state in
  FunctionLiteral { params; body }

and block state =
  ignore (expect_specific state Lbrace "Expected '{' to start block");
  let rec gather acc =
    skip_newlines state;
    match peek state with
    | { kind = Rbrace; _ } ->
        ignore (advance state);
        List.rev acc
    | { kind = Eof; _ } -> fail state "Unterminated block"
    | _ ->
        let stmt = declaration state in
        gather (stmt :: acc)
  in
  gather []

and statement state =
  match peek state with
  | { kind = Window_kw; _ } ->
      let gui = Gui_parser.parse_root state in
      Gui gui
  | { kind = Match_kw; _ } ->
      ignore (advance state);
      parse_match state
  | { kind = If_kw; _ } ->
      ignore (advance state);
      parse_if state
  | { kind = While_kw; _ } ->
      ignore (advance state);
      parse_while state
  | { kind = For_kw; _ } ->
      ignore (advance state);
      parse_for state
  | { kind = Return_kw; _ } ->
      ignore (advance state);
      parse_return state
  | { kind = Break_kw; _ } ->
      ignore (advance state);
      consume_statement_terminator state;
      Break
  | { kind = Continue_kw; _ } ->
      ignore (advance state);
      consume_statement_terminator state;
      Continue
  | { kind = Try_kw; _ } ->
      ignore (advance state);
      parse_try state
  | { kind = Import_kw; _ } ->
      ignore (advance state);
      parse_import state
  | _ -> expression_or_assignment state

and parse_import state =
  let module_name =
    match peek state with
    | { kind = String_lit path; _ } ->
        ignore (advance state);
        path
    | { kind = Ident name; _ } ->
        ignore (advance state);
        name
    | _ -> fail state "Expected a module name or path after 'import'"
  in
  consume_statement_terminator state;
  Import module_name

and parse_if state =
  let condition = parse_condition state in
  let then_block = block state in
  skip_newlines state;
  let else_branch =
    match peek state with
    | { kind = Else_kw; _ } ->
        ignore (advance state);
        skip_newlines state;
        Some
          (match peek state with
          | { kind = If_kw; _ } ->
              ignore (advance state);
              [ parse_if state ]
          | _ -> block state)
    | _ -> None
  in
  IfStmt (condition, then_block, else_branch)

and parse_match state =
  let target = parse_condition state in
  skip_newlines state;
  ignore (expect_specific state Lbrace "Expected '{' to start match block");
  let rec gather cases default_case =
    skip_newlines state;
    match peek state with
    | { kind = Case_kw; _ } ->
        ignore (advance state);
        let case_expr = expression state in
        skip_newlines state;
        let case_block = block state in
        gather ((case_expr, case_block) :: cases) default_case
    | { kind = Default_kw; _ } ->
        if Option.is_some default_case then fail state "Duplicate default case in match";
        ignore (advance state);
        skip_newlines state;
        let default_block = block state in
        gather cases (Some default_block)
    | { kind = Rbrace; _ } ->
        ignore (advance state);
        (List.rev cases, default_case)
    | { kind = Eof; _ } -> fail state "Unterminated match block"
    | _ -> fail state "Expected 'case' or 'default' in match"
  in
  let cases, default_case = gather [] None in
  MatchStmt (target, cases, default_case)

and parse_while state =
  let condition = parse_condition state in
  let body = block state in
  WhileStmt (condition, body)

(* The parentheses around a for header are optional since 0.5, matching if and
   while which already accepted both forms. Writing one without the other is
   still an error. *)
and parse_for state =
  let parenthesised = match_token state (function Lparen -> true | _ -> false) in
  let iterator =
    match expect state (function Ident _ -> true | _ -> false) "Expected iterator name" with
    | { kind = Ident name; _ } -> name
    | _ -> assert false
  in
  ignore (expect_specific state In_kw "Expected 'in' in for loop");
  let iterable = expression state in
  if parenthesised then
    ignore (expect_specific state Rparen "Expected ')' to close for header");
  let body = block state in
  ForStmt (iterator, iterable, body)

(* try { ... } else { ... }
   try { ... } else message { ... }
   No catch keyword, no parentheses: the optional name simply binds the error
   record for the handler block. *)
and parse_try state =
  let attempted = block state in
  ignore (expect_specific state Else_kw "Expected 'else' after a try block");
  let binding =
    match peek state with
    | { kind = Lbrace; _ } -> None
    | token -> (
        match field_name_of_token token with
        | Some name ->
            ignore (advance state);
            Some name
        | None -> fail state "Expected a name or '{' after 'else'")
  in
  let handler = block state in
  TryStmt (attempted, binding, handler)

and parse_return state =
  let value =
    match peek state with
    | { kind = Semicolon | Rbrace | Eof; _ } -> None
    | _ -> Some (expression state)
  in
  consume_statement_terminator state;
  Return value

and expression_or_assignment state =
  let expr = expression state in
  match peek state with
  | { kind = Assign; _ } ->
      ignore (advance state);
      let value = expression state in
      consume_statement_terminator state;
      Assign (expr, value)
  | _ ->
      consume_statement_terminator state;
      ExprStmt expr

and expression state = if_expression state

and if_expression state =
  match peek state with
  | { kind = If_kw; _ } ->
      ignore (advance state);
      let condition = parse_condition state in
      ignore (expect_specific state Then_kw "Expected 'then' in conditional expression");
      let then_expr = expression state in
      ignore (expect_specific state Else_kw "Expected 'else' in conditional expression");
      let else_expr = expression state in
      IfExpr (condition, then_expr, else_expr)
  | _ -> range_expression state

and range_expression state =
  let expr = or_expression state in
  match peek state with
  | { kind = Range; _ } ->
      ignore (advance state);
      let right = or_expression state in
      Range (expr, right)
  | _ -> expr

and or_expression state =
  let rec loop left =
    match peek state with
    | { kind = Or_kw; _ } ->
        ignore (advance state);
        let right = and_expression state in
        loop (Binary (left, B_or, right))
    | _ -> left
  in
  loop (and_expression state)

and and_expression state =
  let rec loop left =
    match peek state with
    | { kind = And_kw; _ } ->
        ignore (advance state);
        let right = equality_expression state in
        loop (Binary (left, B_and, right))
    | _ -> left
  in
  loop (equality_expression state)

and equality_expression state =
  let rec loop left =
    match peek state with
    | { kind = Eq_eq; _ } ->
        ignore (advance state);
        let right = union_expression state in
        loop (Binary (left, B_eq, right))
    | { kind = Bang_eq; _ } ->
        ignore (advance state);
        let right = union_expression state in
        loop (Binary (left, B_neq, right))
    | _ -> left
  in
  loop (union_expression state)

and union_expression state =
  let rec loop left =
    match peek state with
    | { kind = Pipe; _ } ->
        ignore (advance state);
        let right = intersection_expression state in
        loop (Binary (left, B_union, right))
    | _ -> left
  in
  loop (intersection_expression state)

and intersection_expression state =
  let rec loop left =
    match peek state with
    | { kind = Ampersand; _ } ->
        ignore (advance state);
        let right = comparison_expression state in
        loop (Binary (left, B_intersection, right))
    | _ -> left
  in
  loop (comparison_expression state)

and comparison_expression state =
  let rec loop left =
    match peek state with
    | { kind = Lt; _ } ->
        ignore (advance state);
        let right = term state in
        loop (Binary (left, B_lt, right))
    | { kind = Le; _ } ->
        ignore (advance state);
        let right = term state in
        loop (Binary (left, B_le, right))
    | { kind = Gt; _ } ->
        ignore (advance state);
        let right = term state in
        loop (Binary (left, B_gt, right))
    | { kind = Ge; _ } ->
        ignore (advance state);
        let right = term state in
        loop (Binary (left, B_ge, right))
    | _ -> left
  in
  loop (term state)

and term state =
  let rec loop left =
    match peek state with
    | { kind = Plus; _ } ->
        ignore (advance state);
        let right = factor state in
        loop (Binary (left, B_plus, right))
    | { kind = Minus; _ } ->
        ignore (advance state);
        let right = factor state in
        loop (Binary (left, B_minus, right))
    | { kind = At; _ } ->
        ignore (advance state);
        let right = factor state in
        loop (Binary (left, B_at, right))
    | _ -> left
  in
  loop (factor state)

and factor state =
  let rec loop left =
    match peek state with
    | { kind = Star; _ } ->
        ignore (advance state);
        let right = power state in
        loop (Binary (left, B_mul, right))
    | { kind = Slash; _ } ->
        ignore (advance state);
        let right = power state in
        loop (Binary (left, B_div, right))
    | { kind = Percent; _ } ->
        ignore (advance state);
        let right = power state in
        loop (Binary (left, B_mod, right))
    | _ -> left
  in
  loop (power state)

and power state =
  let base = unary state in
  match peek state with
  | { kind = Pow; _ } ->
      ignore (advance state);
      let exponent = power state in
      Binary (base, B_pow, exponent)
  | _ -> base

and unary state =
  match peek state with
  | { kind = Bang; _ } ->
      ignore (advance state);
      Unary (U_not, unary state)
  | { kind = Minus; _ } ->
      ignore (advance state);
      Unary (U_neg, unary state)
  | { kind = Plus; _ } ->
      ignore (advance state);
      Unary (U_pos, unary state)
  | { kind = At; _ } ->
      ignore (advance state);
      Unary (U_pull, unary state)
  | _ -> postfix state

and postfix state =
  let rec loop expr =
    match peek state with
    | { kind = Inc; _ } ->
        ignore (advance state);
        loop (Postfix (Post_inc, expr))
    | { kind = Dec; _ } ->
        ignore (advance state);
        loop (Postfix (Post_dec, expr))
    | _ -> expr
  in
  loop (call state)

and call state =
  let rec loop expr =
    match peek state with
    | { kind = Lparen; _ } ->
        ignore (advance state);
        let args =
          let rec gather args =
            match peek state with
            | { kind = Rparen; _ } ->
                ignore (advance state);
                List.rev args
            | _ ->
                let arg = expression state in
                (match peek state with
                | { kind = Comma; _ } ->
                    ignore (advance state);
                    gather (arg :: args)
                | { kind = Rparen; _ } ->
                    ignore (advance state);
                    List.rev (arg :: args)
                | _ -> fail state "Expected ',' or ')' in call arguments")
          in
          match peek state with
          | { kind = Rparen; _ } ->
              ignore (advance state);
              []
          | _ -> gather []
        in
        loop (Call (expr, args))
    (* Postfix '[': indexing. A '[' that starts a statement is a list literal
       instead, which the mandatory ';' now keeps unambiguous. *)
    | { kind = Lbracket; _ } ->
        ignore (advance state);
        let index = expression state in
        ignore (expect_specific state Rbracket "Expected ']' after index");
        loop (Index (expr, index))
    | { kind = Dot; _ } ->
        ignore (advance state);
        let attr =
          match field_name_of_token (peek state) with
          | Some name ->
              ignore (advance state);
              name
          | None -> fail state "Expected attribute name after '.'"
        in
        loop (Attribute (expr, attr))
    | _ -> expr
  in
  loop (primary state)

and primary state =
  match peek state with
  | { kind = Int_lit value; _ } ->
      ignore (advance state);
      Int value
  | { kind = Float_lit value; _ } ->
      ignore (advance state);
      Float value
  | { kind = True_kw; _ } ->
      ignore (advance state);
      Bool true
  | { kind = False_kw; _ } ->
      ignore (advance state);
      Bool false
  | { kind = None_kw; _ } ->
      ignore (advance state);
      Null
  | { kind = Nan_kw; _ } ->
      ignore (advance state);
      Float Float.nan
  | { kind = Inf_kw; _ } ->
      ignore (advance state);
      Float Float.infinity
  | { kind = String_lit value; _ } ->
      ignore (advance state);
      String value
  (* Record literal: { name: "Emilio"; age: 21; }
     Only reachable from expression position, where a '{' could not previously
     appear, so there is no ambiguity with a block. Keys may be identifiers or
     strings; the latter is what lets json.decode round-trip arbitrary keys. *)
  | { kind = Lbrace; _ } ->
      ignore (advance state);
      let rec fields acc =
        match peek state with
        | { kind = Rbrace; _ } ->
            ignore (advance state);
            Record (List.rev acc)
        | token -> (
            match field_name_of_token token with
            | None -> fail state "Expected a field name or '}' in record literal"
            | Some name ->
                ignore (advance state);
                ignore (expect_specific state Colon "Expected ':' after field name");
                let value = expression state in
                ignore (expect_specific state Semicolon "Expected ';' after record field");
                fields ((name, value) :: acc))
      in
      fields []
  | { kind = Set_kw; _ } ->
      ignore (advance state);
      skip_newlines state;
      ignore (expect_specific state Lbrace "Expected '{' after 'set'");
      let rec elements acc =
        skip_newlines state;
        match peek state with
        | { kind = Rbrace; _ } ->
            ignore (advance state);
            Set (List.rev acc)
        | _ ->
            let value = expression state in
            skip_newlines state;
            (match peek state with
            | { kind = Comma; _ } ->
                ignore (advance state);
                elements (value :: acc)
            | { kind = Rbrace; _ } ->
                ignore (advance state);
                Set (List.rev (value :: acc))
            | _ -> fail state "Expected ',' or '}' in set literal")
      in
      (match peek state with
      | { kind = Rbrace; _ } ->
          ignore (advance state);
          Set []
      | _ -> elements [])
  | { kind = Ident name; _ } ->
      ignore (advance state);
      Identifier name
  | { kind = Lbracket; _ } ->
      ignore (advance state);
      let rec elements acc =
        match peek state with
        | { kind = Rbracket; _ } ->
            ignore (advance state);
            List (List.rev acc)
        | _ ->
            let value = expression state in
            (match peek state with
            | { kind = Comma; _ } ->
                ignore (advance state);
                elements (value :: acc)
            | { kind = Rbracket; _ } ->
                ignore (advance state);
                List (List.rev (value :: acc))
            | _ -> fail state "Expected ',' or ']' in list literal")
      in
      (match peek state with
      | { kind = Rbracket; _ } ->
          ignore (advance state);
          List []
      | _ -> elements [])
  | { kind = Lparen; _ } ->
      ignore (advance state);
      let expr = expression state in
      ignore (expect_specific state Rparen "Expected ')' to close expression");
      expr
  | { kind = Fun_kw; _ } ->
      ignore (advance state);
      parse_function_literal state
  | tok ->
      let message =
        match tok.lexeme with
        | Some lexeme -> Printf.sprintf "Unexpected token '%s'" lexeme
        | None -> "Unexpected token"
      in
      fail state message

and parse_condition state =
  match peek state with
  | { kind = Lparen; _ } ->
      ignore (advance state);
      let expr = expression state in
      ignore (expect_specific state Rparen "Expected ')' after condition");
      expr
  | _ -> expression state

(* Since 0.5 every statement ends with ';'. Statements that end in a block
   (if / while / for / fun / match / window) never reach this function, exactly
   as in C and C#. *)
and consume_statement_terminator state =
  match peek state with
  | { kind = Semicolon; _ } -> ignore (advance state)
  | _ -> fail_missing_semicolon state

(* Reported at the end of the statement that lacks the ';', not at the start of
   whatever came next -- that is where the reader needs to type it. *)
and fail_missing_semicolon state =
  let prev = previous state in
  let width =
    match (prev.kind, prev.lexeme) with
    | String_lit _, Some text -> String.length text + 2 (* the quotes *)
    | _, Some text -> String.length text
    | _, None -> 1
  in
  raise
    (Parse_error
       (Printf.sprintf "Missing ';' at end of statement (line %d, column %d)" prev.line
          (prev.column + width)))
