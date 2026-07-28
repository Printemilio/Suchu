# Changelog

## 0.5

A breaking release. Every `.suchu` file written for 0.4 needs migrating; the
script below does it mechanically.

```bash
node tools/migrate-semicolons.mjs --write examples
```

### Breaking

- **Statements end with `;`.** In exchange, newlines carry no meaning at all, so
  an expression can be wrapped and indented however you like — which is what
  0.4 could not do. Statements that end in a block (`if`, `while`, `for`, `fun`,
  `match`, `Window`) need no terminator, as in C and C#.
- **GUI syntax.** A window is now `Window("Title", 400px, 300px) { … }` with
  capitalised element names and CSS-like properties. `layout: column;` replaces
  the wrapping container the old form required.
- **An unknown GUI element is an error.** It previously became a button in
  silence, so `Labell { … }` produced a stray widget instead of a diagnostic.
- **`;` is required after a GUI property**, matching record literals, which have
  the same shape.

### Added

- **Records.** `user = { name: "Emilio"; age: 21; };` with `user.name`,
  `user["name"]`, field assignment, nesting, and field creation on the fly.
  Insertion order is preserved; equality ignores it.
- **JSON objects** decode into records and encode back, so `decode(encode(x))`
  round-trips. This was the blocker noted in 0.4's `json` module.
- **Indexing.** `items[0]` and `user["key"]` read and write, on lists, strings
  (read-only) and records. Reading an element no longer removes it.
- **`try { … } else [name] { … }`** for recoverable failure. The error is a
  record, so it can gain fields without breaking existing handlers.
- **`break` and `continue`**, applying to the innermost loop.
- **`len(value)`** for strings, lists, sets and records. Unlike `Length.name` it
  takes any expression. `Length.name` still works but is deprecated.
- Parentheses are optional on `for`, matching `if` and `while`.
- A **browser playground** running the real evaluator, compiled with
  js_of_ocaml.

### Fixed

- **`input()` was unreachable.** The documented built-in existed in the runtime,
  but the lexer consumed `input` as a keyword before the parser could see a
  call, so `nom = input("Name: ")` had never parsed.
- `row`, `column`, `label`, `button`, `input` and `id` are no longer reserved
  words and can be used as ordinary identifiers. Only `window` still triggers
  GUI parsing.
- Field names live in their own namespace: any word, reserved or not, works as a
  record field or after a `.`, so `config.window` is legal.
- **Source excerpts never appeared** for `run`, `comp` or `build`. The error
  reporter was handed the file *path* where it expected the file *contents*, so
  it quoted the wrong text or nothing at all.
- A missing terminator now reports where the `;` belongs:

  ```
  Missing ';' at end of statement (line 3, column 15)
    3 | nom = "Emilio"
      |               ^
  ```

### Internal

- `interpreter.ml` was split into a GUI-free evaluator (`suchu_eval`) and a
  Bogue backend (`gui_backend.ml`) that registers itself through
  `Interpreter.gui_hook`. This is what allows the evaluator to be compiled to
  JavaScript without pulling in SDL.

## 0.4

First public shape of the language: expression grammar with postfix operators,
ranges, lists, sets and first-class functions; a module system covering `.suchu`
sources and native OCaml extensions; a CLI with `run`, `build`, `comp` and
`repl`; and a declarative GUI layer backed by Bogue.
