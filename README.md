# Suchu

A small language for native desktop applications, implemented in OCaml 5.

Write an application in one file and ship it as a single native binary. The
window, the buttons and the layout are part of the language — there is no main
loop to write, no runtime to install on the target machine, and no bundled
browser.

```suchu
counter = 0;

Window("Counter", 320px, 200px) {
  background: #1e1e1e;
  layout: column;
  padding: 20px;

  Label { id: "display"; text: "Count: 0"; }

  Button {
    text: "Increment";
    onClick: {
      counter = counter + 1;
      set_text(display, "Count: " @ counter);
    };
  }
}
```

```bash
suchu run counter.suchu          # interpret it
suchu build counter.suchu -o app # one native executable
```

There is also a browser playground that runs the real lexer, parser and
evaluator, compiled to JavaScript with js_of_ocaml. It lives with the website,
in its own repository.

## Install

Requires OCaml 5.1+, [dune](https://dune.build/) and the
[Bogue](https://github.com/sanette/bogue) bindings.

```bash
opam install dune bogue
dune build
./install.sh                     # PREFIX=/custom/path to override
```

The native build target is Linux and WSL. A Windows `.exe` target needs a
Windows OCaml and Bogue toolchain, which is maintained separately.

## The language in one page

```suchu
// Every statement ends with ';'. Newlines carry no meaning, so wrap freely.
total = compute(a) +
        compute(b);

// Records: named fields, no declaration needed
user = { name: "Emilio"; age: 21; };
user.age = 22;
user.city = "Paris";

// Indexing, on lists, strings and records
xs = [10, 20, 30];
print(xs[0]);
print(user["name"]);

// JSON objects are records, so the round trip is faithful
import json;
print(json.decode(json.encode(user)) == user);

// Recoverable failure, without exceptions
try {
  config = read_file("config.json");
} else e {
  print("Using defaults: " @ e.message);
}

// Loops, with break and continue
for i in 1..10 {
  if i > 5 { break; }
  if i % 2 == 0 { continue; }
  print(i);
}
```

Sets, closures, `map` / `filter` / `reduce`, pattern matching with `match`,
ranges, and a module system covering both `.suchu` sources and native OCaml
extensions are all documented on the site.

## Documentation

The reference documentation — language guide, standard library, GUI DSL and a
browser playground — is a separate static site, kept in its own repository
because it ships on a different schedule.

What lives here:

- [`CHANGELOG.md`](CHANGELOG.md) — what changed, and how to migrate.
- [`examples/`](examples/) — runnable programs, `examples/gui/` for windowed
  ones.

Every code block on the documentation site is checked against this parser
before publication, so the two cannot drift apart silently.

## CLI

```bash
suchu run program.suchu             # interpret
suchu build program.suchu -o app    # native executable, imports bundled in
suchu comp program.suchu output.ml  # emit an OCaml stub embedding the program
suchu repl                          # interactive session
suchu update [--skip-deps]          # rebuild and reinstall the CLI
```

Exit codes: `0` success, `1` usage error, `2` parse error, `3` runtime error.

## Repository layout

| Path | Contents |
|------|----------|
| `src/lexical.ml` | Lexer |
| `src/parser.ml`, `src/gui_parser.ml` | Recursive-descent parser, including the GUI grammar |
| `src/runtime.ml` | Values, environments, built-ins |
| `src/interpreter.ml` | The evaluator — deliberately free of any GUI dependency |
| `src/gui_backend.ml` | Bogue backend, installed into the evaluator at startup |
| `src/cli.ml` | CLI driver |
| `web/` | js_of_ocaml entry point for the browser playground |
| `examples/` | Sample programs, `examples/gui/` for windowed ones |
| `tests/` | Test suite, run with `dune test` |
| `tools/` | `migrate-semicolons.mjs`, the 0.4 → 0.5 source migrator |

The library split matters: `suchu_core` (lexer, parser, runtime) and
`suchu_eval` (the evaluator) carry no GUI dependency, which is what lets the
same evaluator compile to JavaScript. `Gui_backend` registers itself through
`Interpreter.gui_hook` at startup, so a build without it simply refuses to open
a window instead of failing to link.

## Development

```bash
dune build
dune test
dune exec suchu-cli -- run examples/basic.suchu
```

## Licence

[MIT](LICENSE) © 2026 Emilio Decaix-Massiani
