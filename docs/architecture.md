# Suchu architecture

Suchu follows a small-core design. Features that define the language stay in
the core; application capabilities live in importable modules.

## Layers

1. `suchu.core` contains the lexer, AST, parser, values, environments, standard
   modules, and the public native extension API. It has no GUI dependency.
2. `suchu.eval` contains the evaluator (`interpreter.ml`). Also GUI-free: the
   `window` statement is dispatched through `Interpreter.gui_hook`, a reference
   a backend fills in at startup.
3. `suchu` contains the Bogue desktop backend (`gui_backend.ml`) and the CLI
   driver. `Gui_backend.register ()` installs the hook and the widget built-ins.
4. `suchu-cli` provides `run`, `repl`, `comp`, and `build`.
5. Application code is split into `.suchu` source modules and optional OCaml
   native modules.

This boundary is load-bearing, not aspirational. Because the evaluator never
mentions Bogue, `web/suchu_web.ml` compiles the same evaluator to JavaScript
with js_of_ocaml for the browser playground, and a build without a backend
raises `GUI support is not available in this build` rather than failing to link.
Widgets cross the runtime as opaque `V_object` values, so SDL types never leak
into the core.

## Native OCaml modules

An OCaml library can expose focused capabilities without adding syntax or
built-ins to Suchu:

```ocaml
open Native_api

let () =
  register_module "crypto"
    [
      function1 "hash" (fun value ->
        string (Digest.to_hex (Digest.string (as_string value))));
      ("backend", string "OCaml");
    ]
```

Suchu code then uses the normal module mechanism:

```suchu
import crypto
print(crypto.hash("hello"))
```

The API provides constructors (`int`, `float`, `bool`, `string`, `list`,
`set`, `null`, `object_value`), checked conversions (`as_int`, `as_float`,
`as_bool`, `as_string`, `as_list`), and helpers for functions with zero, one,
or two arguments.

## Native builds

`suchu build source.suchu -o output`:

1. parses the main source;
2. follows and validates source imports;
3. embeds every imported `.suchu` file;
4. generates a small OCaml entry point;
5. links it with the native Suchu/Bogue runtime.

The result does not need the Suchu CLI or the original source files. The
current backend targets Linux/WSL and still depends on the platform SDL/Bogue
runtime assets. A Windows-native backend and application packaging are
separate targets rather than hidden behind a misleading `.exe` extension.

## Deliberate next boundaries

- Record/map values, then full JSON objects and application models.
- HTTP and async task modules, implemented as extensions rather than syntax.
- Reusable GUI components and dynamic collections.
- Windows-native cross compilation and packaging of SDL/theme assets.
- A package manifest and dependency resolver once the module API is stable.
