# Changelog

## 0.8 (unreleased)

Two releases in one. The renderer moved from Bogue to raylib, and then the
language learned to compile itself. There was never a public 0.7 — what was
written up under that number ships here.

**Two things change behaviour on purpose.** `eval` no longer runs in the
caller's scope, and `build` and `comp` swapped jobs. Both are described under
*Breaking* below.

### The compiler

`suchu comp` translates a program into OCaml and compiles that, leaving a native
binary. It used to package the interpreter with the source and call it
compilation.

- **The whole language compiles.** Numbers, strings, collections, records,
  ranges, indexing, methods, `for`, `match`, `try`, closures, imports, and
  windows. An imported `.suchu` file is translated along with the program and
  travels inside the binary, so nothing has to ship beside it.
- **On a loop of integer arithmetic, about three hundred times the interpreter,
  fifty-six times CPython, and within six per cent of the same loop in C** when
  both compilers are given work neither can shortcut. Startup is about 0.03 s
  against CPython's 0.14 s.
- **`suchu build` writes the OCaml out** instead of compiling it. It is meant to
  be read: your variables keep their names and the shape of the program
  survives.
- **Integers and floats lose their box.** A variable every assignment to which is
  provably a number of one kind becomes a native one. The box returns at the
  edges — an argument, a list, a concatenation — and nowhere else.
- **`suchu comp` needs neither dune nor the Suchu sources.** It calls
  `ocamlfind` and works in any directory, once `opam install .` has put the
  library in the switch.
- **Interpreted and compiled are held to each other.** Every program in
  `tests/differential` runs both ways with the outputs compared as bytes, and
  the generated code calls the same operator functions the interpreter calls, so
  there is one definition of `+` rather than two. That suite found a prefix `-`
  that turned `-0.0` into `0.0`, operands evaluated right to left, and a function
  with no `return` handing back its last statement instead of `none`.

### Drawing

- **`Canvas`**, a rectangle the program paints itself through an `onDraw`
  handler, with a `draw` module: `clear`, `rect`, `circle`, `line`, `text`,
  and the batched `rects`, `circles`, `lines`, `pixels`. Each batched form takes
  a flat list of numbers, so ten thousand shapes cost one call rather than ten
  thousand.
- **`Scene`**, the same with a camera and depth, with a `scene` module: `camera`,
  `light`, `ambient`, `floor`, `cube`, `sphere`, `cylinder`. Lit by a real
  shader — up to four lights, diffuse and specular. A shape that is not fully
  opaque is drawn after the solid ones, furthest away first, with lit edges: it
  reads as glass, but it is transparency and not refraction. No shadows, no
  reflections.
- `examples/gui/sablesim.suchu`, `orbits.suchu` and `scene3d.suchu`.

### Breaking

- **`eval` no longer runs in the caller's scope.** It sees the built-ins that
  only compute and nothing else — not the surrounding variables, not the file
  system, not the keyboard, and it cannot `import`. To let a snippet affect the
  program, hand it a record; its fields are what the snippet may read and write,
  and what it wrote comes back through it.

  This is not a restriction for its own sake. The old behaviour could not
  survive compilation — a caller's variables are entries in an environment when
  interpreted and machine registers when compiled — so the same program printed
  different answers depending on how it was run, silently. Removing the ambient
  access removed the difference.

  A snippet that does not parse now raises an ordinary Suchu error, catchable
  with `try`, rather than ending the program.

- **`build` and `comp` swapped jobs.** `comp` produces the binary; `build`
  writes the OCaml. The old `comp` behaviour — embedding the source and linking
  the interpreter — is `comp --bundle`.

### Added

- Trigonometry and the rest of the usual maths: `sin`, `cos`, `tan`, `asin`,
  `acos`, `atan`, `atan2`, `log`, `exp`, `floor`, `ceil`, and `pi`.
- A chapter on the compiler in the documentation, and the `Canvas`, `Scene`,
  `draw` and `scene` sections in the GUI chapter.

### Fixed

- **Building a list was quadratic.** `push` copied the whole list each time, so
  eighty thousand appends took a minute. A list is a growable array now, and the
  same eighty thousand take 0.008 s. This was in the runtime, so the interpreter
  had it too.
- `&&` and `||` evaluated both sides before consulting the operator, so the
  commonest guard in programming — check a length, then index — raised instead
  of short-circuiting.
- `2 ** 3` was `8.0`. Integer powers stay integers, falling back to a float only
  on a negative exponent or an overflow.
- A condition beginning with `(` was taken to end there, so `if (a + b) % 2 == 0`
  could not be written at all.
- `%` by zero raised OCaml's own error, which no Suchu program could catch.

### Changed

- **Bogue is replaced by raylib.** Bogue drew its own widgets but resisted
  styling, which undercut the one thing the language promises. raylib draws
  nothing at all, so measuring, stacking, placing, clipping, focus and text
  editing are now written here. In exchange, rounded corners, hover states and
  real typography become possible. New dependency: `raylib` instead of
  `bogue`, plus an OpenGL 3.3 driver.
- **`Html` shows its markup as written.** Bogue rendered a small subset of it;
  that is not reimplemented.
- **`Checkbox` draws its text as a label**, where Bogue ignored it.
- **Accented and Latin Extended text works everywhere**, typing included: the
  caret steps over whole characters rather than bytes.

### Fixed

- **`&&` and `||` did not short-circuit.** Both operands were evaluated before
  the operator was consulted, so the commonest guard in programming —
  `len(xs) > 0 && xs[0] == 1` — raised on an empty list instead of stopping at
  the first test. The documentation had always claimed short-circuiting; the
  implementation never did it. Two regression tests now cover it.

- **`2 ** 3` gave `8.0`.** Exponentiation always went through floats. Two
  integers now give an integer, exactly; a negative exponent or a result past
  the 63-bit range still gives a float rather than wrapping into nonsense.
- **`x % 0` killed the program.** It raised OCaml's `Division_by_zero`, which
  escaped as a fatal error `try` could not catch. It now reports `Modulo by
  zero`.
- **`inf` and `NaN` printed as `inf.0` and `nan.0`**, text the language cannot
  read back.
- **Whole floats printed as `4.` instead of `4.0`.** The code tested for a dot
  before appending one, but `string_of_float` always leaves a dot, so the
  branch was dead.
- **`<` `<=` `>` `>=` raised on strings**, despite the documentation promising
  string comparison. They now order strings and booleans too, while numbers
  keep IEEE behaviour: any comparison against `NaN` is `false`.
- **The euro sign and the bullet were missing from the embedded font.** The
  glyph set stopped at Latin Extended-A (U+017F), so `€` and `•` drew as `?` —
  found when Emilio ran his own invoicing program. The atlas now also carries
  typographic punctuation, every currency sign, and the four arrows.
- **`input()` on a closed stdin** raised a bare OCaml `End_of_file` that escaped
  as `Fatal error: exception End_of_file`. It now reports a Suchu runtime error
  a program can catch and a reader can understand.

### Added

- **Suchu compiles to OCaml.** `suchu build` writes readable OCaml; `suchu comp`
  performs the same translation in a directory it discards and hands the result
  to `ocamlopt`. `suchu run` is unchanged. On ten million loop iterations:
  CPython 3.10 takes 1.1 s, the Suchu interpreter 5.6 s, and the compiled
  binary **0.25 s** — twenty-two times the interpreter, four and a half times
  CPython. The binary is 3.8 MB instead of 12, carrying no lexer or evaluator.
  The generated code calls the same operator functions the evaluator calls, so
  arithmetic cannot disagree between the two. Covers numbers, strings,
  variables, operators, `if`, `while`, `break`, `continue`, functions and
  calls; anything else fails at compile time naming what is missing, and
  `suchu comp --bundle` keeps the old behaviour of shipping the interpreter
  with the program — still the only way to build a GUI program.

- **Windows are resizable, and the layout follows.** The size in the header is
  a floor: the window grows freely but never shrinks below it, so it cannot be
  dragged smaller than its own contents. `width: 50%`, `height:
  fill`: a percentage takes a share of what the parent has inside its padding,
  `fill` shares out whatever room is left. `%` stays the modulo operator
  everywhere else — it is a unit only when written tight against a number
  inside a GUI property, so `50%` is a width and `50 % 3` is a remainder.
- **Text properties inherit.** `color`, `font-size`, `font-weight` and
  `text-align` set on a container reach everything inside it, and a child
  overrides them by naming its own. Set them on the `Window` and they are the
  theme for the whole program. Layout and paint properties do not inherit, as
  in CSS.
- **An unknown property is reported** on stderr instead of vanishing, so
  `fint-size` no longer silently leaves an element at its inherited size.
- **`ms` and `s` units**: `interval: 100ms`, `interval: 2s`. A bare number
  still means milliseconds. A suffix only counts as a unit when nothing
  name-like follows, so `100start` is unaffected.
- **`Timer`**, the first element that acts without the user:
  `Timer { interval: 100ms; onTick: { … }; }`. A window holding
  one never sleeps on events, so a stopwatch keeps running when the window
  loses focus.
- **The window costs nothing at rest.** The loop waits for the next event
  instead of redrawing sixty times a second: zero CPU time measured over ten
  idle seconds. It only spins when something has to animate — a blinking
  caret, a drag, a timer.
- **The typeface is embedded in the binary** (Liberation Sans, SIL OFL 1.1,
  825 kB). A built application no longer depends on what is installed on the
  target machine, which is what makes "one executable, nothing to install"
  literally true.
- **Values that used to be hard-coded are properties now**, under CSS names
  wherever CSS has one: `min-height`, `padding-top/-right/-bottom/-left`,
  `hover-background`, `active-background`, `focus-color`, `border-color`,
  `selection-color`, `size`, `track-height`, `knob-size`, `tab-height`,
  `tick-color`. A light, compact theme is written entirely in the `.suchu`
  file.
- **Text selection in `Input`**: by mouse, by `Shift` with the arrows,
  double-click for a word, `Ctrl+A`, `Ctrl+C`, `Ctrl+X`, `Ctrl+V`. Long text
  scrolls sideways to keep the caret in view.
- `text-align` now applies to `Label` and `Text`; it was read and ignored.
- A `Box` fires `onClick` and `onHover`. It accepted handlers before and
  silently dropped them.
- The mouse cursor changes shape over interactive elements.
- A `Select` opens its list upwards when there is no room below.
- The scrollbar can be dragged, and `PageUp` / `PageDown` / arrows / `Home` /
  `End` scroll the box under the pointer.
- `docs/gui-reference.md`: every element, event, property and built-in in one
  page.
- `src/gui_tree.ml`: the GUI tree and its style properties, free of any
  drawing dependency and shared by any backend.

## 0.6

Never given a section here, and never released on its own. Its widgets and
built-ins are described throughout the 0.8 notes above as the state the raylib
backend had to match.

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
