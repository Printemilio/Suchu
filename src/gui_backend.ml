(* The GUI backend, on raylib. New in 0.8; it replaced Bogue outright.

   Bogue drew the widgets for us but resisted styling, which undermined the
   one thing the language promises. raylib draws nothing at all and leaves us
   the pixels, so everything Bogue used to provide -- measuring, stacking,
   placing, clipping, focus, text editing -- lives here instead.

   The language surface did not move with it: this consumes Gui_tree, which is
   backend-neutral, and installs itself into Interpreter.gui_hook. The
   evaluator still carries no GUI dependency, so suchu_eval keeps compiling to
   JavaScript for the web playground.

   Covers every element the language has: Window, Column, Row, Scroll, Label,
   Button, Input, Tabs, Select, Text, Html, Checkbox, Slider, Image, Box,
   Spacer and Shortcut.

   Two behaviours differ from 0.6, both supersets rather than regressions:
   'Html' shows its markup as written instead of rendering the small HTML
   subset Bogue supported, and a 'Checkbox' draws its text as a label where
   Bogue ignored it. *)

open Ast
open Runtime
open Interpreter
open Gui_tree

module R = Raylib

let is_set reference = match !reference with Some _ -> true | None -> false

(* --- fonts ----------------------------------------------------------------

   raylib bakes a fixed glyph set into a texture atlas when the font loads;
   anything outside that set draws as '?'. ASCII alone would break every
   language but English, so we take Latin-1 Supplement and Latin Extended-A
   too: French, German, Spanish, Polish, Czech, Turkish and the rest of
   Latin-script Europe. Cyrillic, Greek and CJK are deliberately out of scope
   for 0.8 -- they need an atlas built on demand, which is its own project. *)

let latin_codepoints =
  let range first last = List.init (last - first + 1) (fun index -> first + index) in
  range 0x20 0x7E (* ASCII *)
  @ range 0xA0 0x17F (* Latin-1 Supplement and Latin Extended-A *)
  (* Typographic punctuation and currency. Latin Extended alone stops at
     U+017F, which leaves out the euro sign and the bullet -- and an invoicing
     application that cannot print '€' is not an invoicing application. Costs a
     few dozen glyphs in the atlas. *)
  @ range 0x2010 0x2027 (* dashes, curly quotes, bullet, ellipsis *)
  @ range 0x2030 0x2044 (* per-mille, primes, fraction slash *)
  @ range 0x20A0 0x20BF (* every currency sign, including U+20AC *)
  @ range 0x2190 0x2193 (* the four arrows *)

(* Loaded well above any size we draw at: reducing a large glyph with a
   bilinear filter stays clean, enlarging a small one does not. *)
let atlas_size = 96

let glyph_spacing = 0.5

(* The typeface travels inside the binary (see Fonts_data, generated from
   assets/fonts), so a built application looks the same on a machine with no
   fonts installed at all -- which is what "ship a single native binary"
   has to mean. The system paths below are only a fallback for the case where
   the embedded face somehow fails to load. *)
let regular_candidates =
  [
    "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf";
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf";
    "/usr/share/fonts/TTF/DejaVuSans.ttf";
    "C:/Windows/Fonts/segoeui.ttf";
  ]

let bold_candidates =
  [
    "/usr/share/fonts/truetype/ubuntu/Ubuntu-B.ttf";
    "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf";
    "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf";
    "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf";
    "C:/Windows/Fonts/segoeuib.ttf";
  ]

type fonts = {
  regular : R.Font.t;
  bold : R.Font.t;
}

(* A failed load still returns a Font, with base_size 0, so success has to be
   checked rather than assumed. *)
let usable font = R.Font.base_size font > 0

let smooth font =
  R.set_texture_filter (R.Font.texture font) R.TextureFilter.Bilinear;
  font

let load_face embedded candidates =
  let codepoints () = R.CArray.of_list Ctypes.int latin_codepoints in
  let from_memory = R.load_font_from_memory ".ttf" embedded atlas_size (codepoints ()) in
  if usable from_memory then smooth from_memory
  else
    match List.find_opt Sys.file_exists candidates with
    | None -> R.get_font_default ()
    | Some path ->
        let font = R.load_font_ex path atlas_size (Some (codepoints ())) in
        if usable font then smooth font else R.get_font_default ()

(* --- UTF-8 ----------------------------------------------------------------

   raylib hands us codepoints, one per keystroke, and the language stores
   strings as bytes. The official raylib text-input example deletes one byte
   per backspace, which corrupts anything outside ASCII -- pressing backspace
   after typing 'é' would leave half a character behind. So the caret is a
   byte offset that only ever lands on a sequence boundary. *)

let is_continuation_byte character = Char.code character land 0xC0 = 0x80

let utf8_prev text index =
  if index <= 0 then 0
  else begin
    let cursor = ref (index - 1) in
    while !cursor > 0 && is_continuation_byte text.[!cursor] do
      decr cursor
    done;
    !cursor
  end

let utf8_next text index =
  let length = String.length text in
  if index >= length then length
  else begin
    let cursor = ref (index + 1) in
    while !cursor < length && is_continuation_byte text.[!cursor] do
      incr cursor
    done;
    !cursor
  end

let utf8_of_uchar uchar =
  let buffer = Buffer.create 4 in
  Buffer.add_utf_8_uchar buffer uchar;
  Buffer.contents buffer

(* --- the retained tree ----------------------------------------------------

   raylib is immediate-mode, but the language is declarative and mutates
   widgets by name afterwards (set_text (display, ...)). So we keep our own
   retained tree and redraw it every frame -- the easy direction. *)

(* A modifier is stored side-agnostic: 'Ctrl+S' must fire from either control
   key, so the check tests both at use time. *)
type modifier =
  | ModCtrl
  | ModShift
  | ModAlt

type rl_kind =
  | RlColumn
  | RlRow
  | RlScroll
  | RlLabel
  | RlButton
  | RlInput
  | RlTabs
  | RlSelect
  (* Multi-line display. 'Html' maps here too: Bogue renders a small subset of
     HTML, and reimplementing that is not a 0.8 job, so the markup shows as
     written rather than being silently dropped. *)
  | RlText
  | RlBox
  | RlSpacer
  | RlImage
  | RlCheckbox
  | RlSlider
  (* Draw nothing: one binds a key, the other fires on a schedule. *)
  | RlShortcut
  | RlTimer
  (* A rectangle the program paints itself, once per frame. *)
  | RlCanvas
  (* The same, with a camera in front of it and depth turned on. *)
  | RlScene

type rl_node = {
  kind : rl_kind;
  id : string option;
  styles : gui_styles;
  events : gui_event list;
  mutable text : string;
  mutable visible : bool;
  (* Filled in by the layout pass, in pixels, absolute to the window. *)
  mutable x : float;
  mutable y : float;
  mutable width : float;
  mutable height : float;
  (* Interaction state carried between frames: [hovered] so onHover fires on
     the transition rather than every frame, [pressed] so a click means press
     and release on the same widget. *)
  mutable hovered : bool;
  mutable pressed : bool;
  (* A disabled control still draws, dimmed, but takes no input. *)
  mutable enabled : bool;
  (* Set by set_width / set_height, which override both the measured size and
     anything the styles asked for -- otherwise the next measure pass would
     undo the call. *)
  mutable forced_width : float option;
  mutable forced_height : float option;
  (* Scroll containers: how far down we are, and how tall the content is. *)
  mutable scroll_y : float;
  mutable content_height : float;
  (* Input: caret and selection anchor, both byte offsets on a UTF-8
     boundary. Equal means no selection. [text_offset] scrolls the text
     sideways so the caret stays inside a field too narrow for it. *)
  mutable caret : int;
  mutable anchor : int;
  mutable text_offset : float;
  (* When this field was last clicked, to tell a double-click from two. *)
  mutable last_click_at : float;
  placeholder : string;
  (* Tabs: one title per child, and which child is showing. *)
  titles : string array;
  mutable active : int;
  mutable hovered_tab : int;
  mutable tab_rects : R.Rectangle.t array;
  (* Select: the entries, and whether the list is dropped down. *)
  options : string array;
  mutable selected : int;
  mutable expanded : bool;
  (* Text: the wrapped lines, computed by the measure pass so the drawing pass
     does not have to redo the same work. *)
  mutable lines : string list;
  (* Checkbox. *)
  mutable checked : bool;
  (* Slider: current position, upper bound, and an optional step to snap to. *)
  mutable slider_value : float;
  slider_max : float;
  slider_step : float option;
  (* Image: the texture is loaded after the window exists, since it needs a GL
     context, so it starts empty even though the path is known here. *)
  mutable texture : R.Texture.t option;
  (* Shortcut: the modifiers and key it binds, parsed once at conversion. *)
  shortcut : (modifier list * R.Key.t) option;
  (* Timer: how long between ticks, in seconds, and when the last one fired. *)
  interval : float;
  mutable last_tick : float;
  children : rl_node list;
}

let make_node ?(text = "") ?(events = []) ?(placeholder = "") ?(titles = [||])
    ?(options = [||]) ?(selected = 0) ?(checked = false) ?(slider_value = 0.)
    ?(slider_max = 100.) ?slider_step ?shortcut ?(interval = 0.) ~kind ~id ~styles children =
  {
    kind;
    id;
    styles;
    events;
    text;
    visible = true;
    x = 0.;
    y = 0.;
    width = 0.;
    height = 0.;
    hovered = false;
    pressed = false;
    enabled = true;
    forced_width = None;
    forced_height = None;
    scroll_y = 0.;
    content_height = 0.;
    caret = String.length text;
    anchor = String.length text;
    text_offset = 0.;
    last_click_at = Float.neg_infinity;
    placeholder;
    titles;
    active = 0;
    hovered_tab = -1;
    tab_rects = [||];
    options;
    selected;
    expanded = false;
    lines = [];
    checked;
    slider_value;
    slider_max;
    slider_step;
    texture = None;
    shortcut;
    interval;
    last_tick = 0.;
    children;
  }

let unsupported element =
  raise (Runtime_error (Printf.sprintf "Unhandled element '%s'" element))

(* Bogue drew its own widgets, so 0.6 scripts could leave them unstyled. Here
   nothing appears unless we draw it, and an unstyled control would be
   invisible -- hence defaults, overridden by anything the script sets. *)
let button_styles styles =
  styles
  |> ensure_style [ "background"; "background-color" ] "background" (GuiColor "#3b82f6")
  |> ensure_style [ "border-radius"; "radius" ] "border-radius" (GuiNumber 10.)
  |> ensure_style [ "padding"; "margin"; "margins" ] "padding" (GuiNumber 14.)
  |> ensure_style [ "color"; "text-color" ] "color" (GuiColor "#ffffff")

let input_styles styles =
  styles
  |> ensure_style [ "background"; "background-color" ] "background" (GuiColor "#24283b")
  |> ensure_style [ "border-radius"; "radius" ] "border-radius" (GuiNumber 10.)
  |> ensure_style [ "padding"; "margin"; "margins" ] "padding" (GuiNumber 12.)
  |> ensure_style [ "height" ] "height" (GuiNumber 46.)
  |> ensure_style [ "width" ] "width" (GuiNumber 260.)

let select_styles styles =
  styles
  |> ensure_style [ "background"; "background-color" ] "background" (GuiColor "#24283b")
  |> ensure_style [ "border-radius"; "radius" ] "border-radius" (GuiNumber 10.)
  |> ensure_style [ "padding"; "margin"; "margins" ] "padding" (GuiNumber 12.)
  |> ensure_style [ "height" ] "height" (GuiNumber 46.)
  |> ensure_style [ "color"; "text-color" ] "color" (GuiColor "#e2e8f0")

let scroll_styles styles = styles |> ensure_style [ "height" ] "height" (GuiNumber 220.)

(* --- keyboard shortcuts ---------------------------------------------------

   Gui_tree keeps the spec as written ("Ctrl+S", "F5"), so each backend maps it
   to its own key codes. Bogue used SDL's; these are raylib's. *)

let letter_keys =
  R.Key.[| A; B; C; D; E; F; G; H; I; J; K; L; M; N; O; P; Q; R; S; T; U; V; W; X; Y; Z |]

let digit_keys = R.Key.[| Zero; One; Two; Three; Four; Five; Six; Seven; Eight; Nine |]

let function_keys = R.Key.[| F1; F2; F3; F4; F5; F6; F7; F8; F9; F10; F11; F12 |]

let key_of_name name =
  let lower = lowercase name in
  if String.length lower = 1 then begin
    let character = lower.[0] in
    if character >= 'a' && character <= 'z' then
      Some letter_keys.(Char.code character - Char.code 'a')
    else if character >= '0' && character <= '9' then
      Some digit_keys.(Char.code character - Char.code '0')
    else None
  end
  else
    match lower with
    | "enter" | "return" -> Some R.Key.Enter
    | "escape" | "esc" -> Some R.Key.Escape
    | "space" -> Some R.Key.Space
    | "tab" -> Some R.Key.Tab
    | "backspace" -> Some R.Key.Backspace
    | "delete" | "del" -> Some R.Key.Delete
    | "insert" -> Some R.Key.Insert
    | "up" -> Some R.Key.Up
    | "down" -> Some R.Key.Down
    | "left" -> Some R.Key.Left
    | "right" -> Some R.Key.Right
    | "home" -> Some R.Key.Home
    | "end" -> Some R.Key.End
    | "pageup" -> Some R.Key.Page_up
    | "pagedown" -> Some R.Key.Page_down
    | _ ->
        if String.length lower >= 2 && lower.[0] = 'f' then
          match int_of_string_opt (String.sub lower 1 (String.length lower - 1)) with
          | Some number when number >= 1 && number <= 12 -> Some function_keys.(number - 1)
          | _ -> None
        else None

let parse_shortcut spec =
  let parts =
    String.split_on_char '+' spec |> List.map String.trim |> List.filter (fun part -> part <> "")
  in
  match List.rev parts with
  | [] -> raise (Runtime_error "Shortcut key cannot be empty")
  | key_name :: modifier_names ->
      let key =
        match key_of_name key_name with
        | Some key -> key
        | None ->
            raise
              (Runtime_error
                 (Printf.sprintf "Unknown key '%s' in shortcut '%s'" key_name spec))
      in
      let modifiers =
        List.map
          (fun name ->
            match lowercase name with
            | "ctrl" | "control" -> ModCtrl
            | "shift" -> ModShift
            | "alt" -> ModAlt
            | other ->
                raise
                  (Runtime_error
                     (Printf.sprintf "Unknown modifier '%s' in shortcut '%s'" other spec)))
          modifier_names
      in
      (modifiers, key)

let reject_event element = function
  | GuiOnClick _
  | GuiOnHover _ -> ()
  | GuiOnInput _ -> raise (Runtime_error (element ^ " does not support onInput"))
  | GuiOnChange _ ->
      raise (Runtime_error (element ^ " does not support onChange; use onClick"))
  | GuiOnTick _ -> raise (Runtime_error (element ^ " does not support onTick; use a Timer"))
  | GuiOnDraw _ -> raise (Runtime_error (element ^ " does not support onDraw; use a Canvas"))

(* A property nobody reads used to vanish without a word, so a single typo --
   'fint-size' for 'font-size' -- silently left the element with whatever it
   inherited. The language already refuses an unknown element rather than
   guessing; this is the same idea one level down, kept to a warning because a
   future backend may well read properties this one does not. *)
let known_properties =
  [
    "id";
    (* box *)
    "width"; "height"; "min-height"; "size"; "box-size";
    "padding"; "padding-top"; "padding-right"; "padding-bottom"; "padding-left";
    "margin"; "margins"; "spacing"; "gap";
    "align"; "alignment"; "justify-content"; "layout";
    (* text *)
    "font-size"; "font-weight"; "weight"; "color"; "text-color";
    "text-align"; "textalign";
    (* paint *)
    "background"; "background-color"; "border-radius"; "radius"; "border-color";
    "focus-color"; "hover-background"; "hover-color"; "active-background";
    "pressed-background"; "selection-color"; "tick-color";
    (* per element *)
    "title"; "text"; "content"; "value"; "label"; "src"; "placeholder"; "prompt";
    "checked"; "state"; "max"; "step"; "track-height"; "knob-size"; "tab-height";
    "interval"; "every"; "period"; "key"; "keys"; "combo"; "selected";
  ]

let checked element styles =
  List.iter
    (fun (name, _) ->
      if not (List.mem (lowercase name) known_properties) then
        Printf.eprintf "GUI warning: unknown property '%s' on %s, ignored\n%!" name element)
    styles;
  styles

(* --- inherited styles -----------------------------------------------------

   As in CSS, only the properties that describe text travel down the tree;
   width, padding and background stay where they are written. Setting them on
   the Window is what makes a theme, without a separate concept for it.

   Bare 'size' is deliberately left out: it is an alias of font-size, but also
   the box of a Checkbox, so inheriting it would shrink checkboxes that never
   asked. *)
let inherited_groups =
  [
    [ "color"; "text-color" ];
    [ "font-size" ];
    [ "font-weight"; "weight" ];
    [ "text-align"; "textalign" ];
  ]

(* A child that names any spelling of a property keeps its own, so the whole
   alias group has to be dropped from what it inherits -- otherwise a parent's
   'color' would still win over a child's 'text-color', since lookup goes by
   name rather than by position. *)
let inherit_into ~inherited own =
  let kept =
    List.concat_map
      (fun group ->
        if List.exists (fun name -> has_any_style [ name ] own) group then []
        else List.filter (fun (name, _) -> List.mem (lowercase name) group) inherited)
      inherited_groups
  in
  own @ kept

let inheritable styles =
  List.filter
    (fun (name, _) -> List.exists (List.mem (lowercase name)) inherited_groups)
    styles

let rec convert ~inherited = function
  | GuiColumn container ->
      let styles = inherit_into ~inherited (checked "Column" container.styles) in
      make_node ~kind:RlColumn ~id:container.id ~styles
        (List.map (convert ~inherited:(inheritable styles)) container.children)
  | GuiRow container ->
      let styles = inherit_into ~inherited (checked "Row" container.styles) in
      make_node ~kind:RlRow ~id:container.id ~styles
        (List.map (convert ~inherited:(inheritable styles)) container.children)
  | GuiScroll container ->
      let styles = inherit_into ~inherited (scroll_styles (checked "Scroll" container.styles)) in
      make_node ~kind:RlScroll ~id:container.id ~styles
        (List.map (convert ~inherited:(inheritable styles)) container.children)
  | GuiLabel label ->
      make_node ~kind:RlLabel ~id:label.id
        ~styles:(inherit_into ~inherited (checked "Label" label.styles))
        ~text:label.text []
  | GuiButton button ->
      List.iter (reject_event "button") button.events;
      make_node ~kind:RlButton ~id:button.id
        ~styles:(inherit_into ~inherited (button_styles (checked "Button" button.styles)))
        ~text:button.label ~events:button.events []
  | GuiInput input ->
      make_node ~kind:RlInput ~id:input.id
        ~styles:(inherit_into ~inherited (input_styles (checked "Input" input.styles)))
        ~placeholder:(Option.value input.placeholder ~default:"")
        ~events:input.events []
  | GuiTabs tabs ->
      let titles = Array.of_list (List.map fst tabs.pages) in
      let styles = inherit_into ~inherited (checked "Tabs" tabs.styles) in
      make_node ~kind:RlTabs ~id:tabs.id ~styles ~titles
        (List.map (fun (_, page) -> convert ~inherited:(inheritable styles) page) tabs.pages)
  | GuiSelect select ->
      make_node ~kind:RlSelect ~id:select.id
        ~styles:(inherit_into ~inherited (select_styles (checked "Select" select.styles)))
        ~options:(Array.of_list select.options)
        ~selected:select.selected ~events:select.events []
  | GuiShortcut shortcut ->
      make_node ~kind:RlShortcut ~id:None ~styles:[] ~events:shortcut.events
        ~shortcut:(parse_shortcut shortcut.key) []
  | GuiSimple simple -> (
      let styles =
        inherit_into ~inherited
          (checked (String.capitalize_ascii simple.kind) simple.styles)
      in
      let number names ~default =
        Option.value
          (Option.bind (find_first_literal names styles) int_from_literal)
          ~default
      in
      match simple.kind with
      | "text"
      | "html" ->
          make_node ~kind:RlText ~id:simple.id ~styles ~text:simple.content []
      | "box" -> make_node ~kind:RlBox ~id:simple.id ~styles ~events:simple.events []
      | "canvas" -> make_node ~kind:RlCanvas ~id:simple.id ~styles ~events:simple.events []
      | "scene" -> make_node ~kind:RlScene ~id:simple.id ~styles ~events:simple.events []
      | "spacer" -> make_node ~kind:RlSpacer ~id:simple.id ~styles []
      | "timer" ->
          (* Milliseconds in the script, seconds internally. Floored at one
             frame: a timer asking for less would just fire every frame. *)
          let millis =
            Option.value
              (Option.bind
                 (find_first_literal [ "interval"; "every"; "period" ] styles)
                 duration_from_literal)
              ~default:1000
          in
          make_node ~kind:RlTimer ~id:simple.id ~styles ~events:simple.events
            ~interval:(Float.max 0.016 (float_of_int millis /. 1000.))
            []
      | "image" -> make_node ~kind:RlImage ~id:simple.id ~styles ~text:simple.content []
      | "checkbox" ->
          let checked =
            Option.value
              (Option.bind (find_first_literal [ "checked"; "state" ] styles) bool_from_literal)
              ~default:false
          in
          make_node ~kind:RlCheckbox ~id:simple.id ~styles ~text:simple.content
            ~events:simple.events ~checked []
      | "slider" ->
          let slider_max = float_of_int (max 1 (number [ "max" ] ~default:100)) in
          let slider_value = float_of_int (number [ "value" ] ~default:0) in
          let slider_step =
            Option.map float_of_int
              (Option.bind (find_literal "step" styles) int_from_literal)
          in
          make_node ~kind:RlSlider ~id:simple.id ~styles ~events:simple.events
            ~slider_value:(Float.min slider_max (Float.max 0. slider_value))
            ~slider_max ?slider_step []
      | other -> unsupported (String.capitalize_ascii other))
  | GuiWindow _ -> raise (Runtime_error "A window cannot be nested inside another window")

(* --- reading styles ------------------------------------------------------- *)

let rgba (red, green, blue, alpha) = R.Color.create red green blue alpha

let default_text_color = R.Color.create 226 232 240 255
let muted_color = R.Color.create 148 163 184 255
let accent_color = R.Color.create 59 130 246 255
let panel_color = R.Color.create 36 40 59 255

let color_or fallback names styles =
  match Option.bind (find_first_literal names styles) color_from_literal with
  | Some color -> rgba color
  | None -> fallback

let background_of styles =
  Option.bind (find_first_literal [ "background"; "background-color" ] styles) color_from_literal
  |> Option.map rgba

let float_style getter styles ~default =
  match getter styles with
  | Some value -> float_of_int value
  | None -> default

(* Without an explicit hover or pressed colour, the base one is shifted. One
   amount for every control, so the feedback reads the same everywhere. *)
let shift_channel amount channel = max 0 (min 255 (channel + amount))

let shift_color amount color =
  R.Color.create
    (shift_channel amount (R.Color.r color))
    (shift_channel amount (R.Color.g color))
    (shift_channel amount (R.Color.b color))
    (R.Color.a color)

let hover_shift = 24
let press_shift = -24

(* A disabled control is drawn at half opacity, so it blends towards whatever
   is behind it. That reads as 'greyed out' on a dark theme and on a light one
   alike, which a fixed grey would not. *)
let dim enabled color =
  if enabled then color
  else R.Color.create (R.Color.r color) (R.Color.g color) (R.Color.b color) (R.Color.a color / 2)

(* Everything a control draws should be reachable as a property, so that a
   theme is written in the .suchu file and not in this module. These two are
   how any new one gets read; the names follow CSS wherever CSS has one. *)
let number_style names styles ~default =
  match Option.bind (find_first_literal names styles) int_from_literal with
  | Some value -> float_of_int value
  | None -> default

let color_style names styles =
  Option.bind (find_first_literal names styles) color_from_literal |> Option.map rgba

(* A hover or pressed shade can be stated outright; failing that the base
   colour is shifted, which is what keeps unstyled controls looking alive. *)
let hover_fill styles base =
  match color_style [ "hover-background"; "hover-color" ] styles with
  | Some color -> color
  | None -> shift_color hover_shift base

let active_fill styles base =
  match color_style [ "active-background"; "pressed-background" ] styles with
  | Some color -> color
  | None -> shift_color press_shift base

let focus_color_of styles =
  Option.value (color_style [ "focus-color" ] styles) ~default:accent_color

let border_color_of styles =
  Option.value (color_style [ "border-color" ] styles) ~default:muted_color

(* 'width: fill' asks for whatever the parent has left, instead of a fixed
   number of pixels. It is what lets a window be resized without the content
   staying the size it happened to start at. Percentages would be closer to
   CSS, but '50%' does not lex today: '%' is the modulo operator. *)
let fills names styles =
  match Option.bind (find_first_literal names styles) text_literal with
  | Some value -> (
      match lowercase value with
      | "fill" | "stretch" | "expand" -> true
      | _ -> false)
  | None -> false

let fills_width node = fills [ "width" ] node.styles
let fills_height node = fills [ "height" ] node.styles

(* A share of whatever the parent has inside its padding, written 50%. Like
   fill, it can only be resolved once the parent is being laid out. *)
let percent_of names styles =
  match find_first_literal names styles with
  | Some (GuiPercent share) -> Some (share /. 100.)
  | _ -> None

(* CSS lets each side be set on its own; padding stays the shorthand that
   fills in whichever side was not named. *)
type edges = {
  e_top : float;
  e_right : float;
  e_bottom : float;
  e_left : float;
}

let padding_edges styles =
  let base = number_style [ "padding"; "margin"; "margins" ] styles ~default:0. in
  {
    e_top = number_style [ "padding-top" ] styles ~default:base;
    e_right = number_style [ "padding-right" ] styles ~default:base;
    e_bottom = number_style [ "padding-bottom" ] styles ~default:base;
    e_left = number_style [ "padding-left" ] styles ~default:base;
  }

let sides pad = pad.e_left +. pad.e_right
let ends pad = pad.e_top +. pad.e_bottom

let font_size_of styles = float_style font_size_from_styles styles ~default:20.
let padding_of styles = float_style margins_from_styles styles ~default:0.
let spacing_of styles = float_style spacing_from_styles styles ~default:0.

let face fonts styles =
  match Option.bind (find_first_literal [ "font-weight"; "weight" ] styles) text_literal with
  | Some value when String.equal (lowercase value) "bold" -> fonts.bold
  | _ -> fonts.regular

let text_color_of styles = color_or default_text_color [ "color"; "text-color" ] styles

(* --- layout ---------------------------------------------------------------

   Two passes, as in any retained toolkit: measure bottom-up so every node
   knows its own size, then place top-down now that sizes are known. *)

(* 44px is the usual minimum click target; a control shorter than that is
   uncomfortable to hit however short its text is. *)
let control_min_height = 44.

let tab_header_padding = 14.

let visible_children node = List.filter (fun child -> child.visible) node.children

let text_extent fonts styles text =
  R.measure_text_ex (face fonts styles) text (font_size_of styles) glyph_spacing

(* Greedy word wrap, and the only place multi-line text is produced. A word
   longer than the whole width is left to overflow rather than being cut
   mid-character, which would break UTF-8. Explicit newlines are kept. *)
let wrap_lines fonts styles text limit =
  let width_of piece = R.Vector2.x (text_extent fonts styles piece) in
  let wrap_paragraph paragraph =
    let words = String.split_on_char ' ' paragraph in
    let lines, last =
      List.fold_left
        (fun (lines, current) word ->
          if String.equal current "" then (lines, word)
          else
            let candidate = current ^ " " ^ word in
            if width_of candidate <= limit then (lines, candidate)
            else (current :: lines, word))
        ([], "") words
    in
    List.rev (if String.equal last "" then lines else last :: lines)
  in
  String.split_on_char '\n' text |> List.concat_map wrap_paragraph

(* Width of the first [count] bytes, which is how every caret and selection
   coordinate is derived. *)
let prefix_width fonts styles text count =
  R.Vector2.x (text_extent fonts styles (String.sub text 0 count))

(* Turn a pixel position into a caret index: walk the sequence boundaries and
   keep the nearest, so clicking the right half of a glyph lands after it. *)
let index_at_x fonts node local_x =
  let text = node.text in
  let rec walk index best best_distance =
    let distance = Float.abs (prefix_width fonts node.styles text index -. local_x) in
    let best, best_distance =
      if distance < best_distance then (index, distance) else (best, best_distance)
    in
    if index >= String.length text then best
    else walk (utf8_next text index) best best_distance
  in
  walk 0 0 infinity

(* Letters, digits and underscore, plus every byte above ASCII so that an
   accented letter stays inside its word rather than splitting it. *)
let is_word_byte character =
  let code = Char.code character in
  code >= 0x80
  || (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')
  || (character >= '0' && character <= '9')
  || character = '_'

let word_bounds text index =
  let length = String.length text in
  let index = min index (max 0 (length - 1)) in
  if length = 0 || not (is_word_byte text.[index]) then (index, index)
  else begin
    let first = ref index and last = ref index in
    while !first > 0 && is_word_byte text.[!first - 1] do
      decr first
    done;
    while !last < length && is_word_byte text.[!last] do
      incr last
    done;
    (!first, !last)
  end

let selection_range node =
  (min node.caret node.anchor, max node.caret node.anchor)

let has_selection node = node.caret <> node.anchor

let apply_explicit_size node =
  Option.iter (fun value -> node.width <- float_of_int value) (width_from_styles node.styles);
  Option.iter (fun value -> node.height <- float_of_int value) (height_from_styles node.styles);
  (* set_width / set_height win over the styles: they were called later. *)
  Option.iter (fun value -> node.width <- value) node.forced_width;
  Option.iter (fun value -> node.height <- value) node.forced_height

(* Defaults only: each of these is a property a script can set. *)
let checkbox_size = 22.
let checkbox_gap = 10.
let slider_default_width = 200.
let slider_default_height = 24.

let checkbox_size_of styles = number_style [ "size"; "box-size" ] styles ~default:checkbox_size
let min_height_of styles ~default = number_style [ "min-height" ] styles ~default

let line_height fonts styles = R.Vector2.y (text_extent fonts styles "Ag") *. 1.35

let rec measure fonts node =
  match node.kind with
  | RlShortcut
  | RlTimer ->
      node.width <- 0.;
      node.height <- 0.
  | RlSpacer ->
      node.width <- float_style width_from_styles node.styles ~default:1.;
      node.height <- float_style height_from_styles node.styles ~default:1.
  | RlBox ->
      node.width <- float_style width_from_styles node.styles ~default:40.
      ;
      node.height <- float_style height_from_styles node.styles ~default:40.
  (* Bigger by default than a Box: a canvas nobody sized is meant to be drawn
     on, and forty pixels square is not worth drawing on. *)
  | RlCanvas | RlScene ->
      node.width <- float_style width_from_styles node.styles ~default:320.;
      node.height <- float_style height_from_styles node.styles ~default:240.
  | RlImage ->
      (* Before the texture exists the styles are all we have; once loaded its
         own size wins unless the script asked for one. *)
      let natural_width, natural_height =
        match node.texture with
        | Some texture ->
            (float_of_int (R.Texture.width texture), float_of_int (R.Texture.height texture))
        | None -> (0., 0.)
      in
      node.width <- float_style width_from_styles node.styles ~default:natural_width;
      node.height <- float_style height_from_styles node.styles ~default:natural_height
  | RlCheckbox ->
      let box = checkbox_size_of node.styles in
      let label_width =
        if String.equal node.text "" then 0.
        else checkbox_gap +. R.Vector2.x (text_extent fonts node.styles node.text)
      in
      node.width <- box +. label_width;
      node.height <- Float.max box (R.Vector2.y (text_extent fonts node.styles "Ag"));
      apply_explicit_size node
  | RlSlider ->
      node.width <- float_style width_from_styles node.styles ~default:slider_default_width;
      node.height <- float_style height_from_styles node.styles ~default:slider_default_height
  | RlText ->
      let limit = Option.map float_of_int (width_from_styles node.styles) in
      node.lines <-
        (match limit with
        | Some limit -> wrap_lines fonts node.styles node.text limit
        | None -> String.split_on_char '\n' node.text);
      let step = line_height fonts node.styles in
      node.width <-
        (match limit with
        | Some limit -> limit
        | None ->
            List.fold_left
              (fun acc line -> Float.max acc (R.Vector2.x (text_extent fonts node.styles line)))
              0. node.lines);
      node.height <- step *. float_of_int (max 1 (List.length node.lines));
      Option.iter
        (fun value -> node.height <- float_of_int value)
        (height_from_styles node.styles)
  | RlLabel ->
      let extent = text_extent fonts node.styles node.text in
      node.width <- R.Vector2.x extent;
      node.height <- R.Vector2.y extent;
      apply_explicit_size node
  | RlButton
  | RlInput
  | RlSelect ->
      let sample =
        match node.kind with
        | RlButton -> node.text
        | RlInput -> if String.equal node.text "" then node.placeholder else node.text
        | _ -> if Array.length node.options = 0 then "" else node.options.(node.selected)
      in
      let extent = text_extent fonts node.styles sample in
      let padding = padding_of node.styles in
      (* The select keeps room for its chevron. *)
      let extra = if node.kind = RlSelect then 28. else 0. in
      node.width <- R.Vector2.x extent +. (2. *. padding) +. extra;
      node.height <-
        Float.max
          (min_height_of node.styles ~default:control_min_height)
          (R.Vector2.y extent +. (2. *. padding));
      apply_explicit_size node
  | RlTabs ->
      List.iter (measure fonts) node.children;
      let header_height =
        number_style [ "tab-height" ] node.styles
          ~default:((font_size_of node.styles *. 1.2) +. (2. *. tab_header_padding))
      in
      let headers_width =
        Array.fold_left
          (fun acc title ->
            acc +. R.Vector2.x (text_extent fonts node.styles title) +. (2. *. tab_header_padding))
          0. node.titles
      in
      let page =
        match List.nth_opt node.children node.active with
        | Some page -> page
        | None -> node
      in
      let page_width = if page == node then 0. else page.width in
      let page_height = if page == node then 0. else page.height in
      node.width <- Float.max headers_width page_width;
      node.height <- header_height +. page_height;
      apply_explicit_size node
  | RlScroll ->
      List.iter (measure fonts) node.children;
      let children = visible_children node in
      let pad = padding_edges node.styles in
      let spacing = spacing_of node.styles in
      let count = List.length children in
      let gaps = if count > 1 then spacing *. float_of_int (count - 1) else 0. in
      let content =
        List.fold_left (fun acc child -> acc +. child.height) 0. children +. gaps +. ends pad
      in
      node.content_height <- content;
      node.width <-
        List.fold_left (fun acc child -> Float.max acc child.width) 0. children +. sides pad;
      (* A scroll box exists to be shorter than its content, so its height
         comes from the styles, never from what it holds. *)
      node.height <- content;
      apply_explicit_size node;
      let overflow = Float.max 0. (node.content_height -. node.height) in
      node.scroll_y <- Float.min node.scroll_y overflow
  | RlColumn
  | RlRow ->
      List.iter (measure fonts) node.children;
      let children = visible_children node in
      let pad = padding_edges node.styles in
      let spacing = spacing_of node.styles in
      let count = List.length children in
      let gaps = if count > 1 then spacing *. float_of_int (count - 1) else 0. in
      let sum get = List.fold_left (fun acc child -> acc +. get child) 0. children in
      let largest get = List.fold_left (fun acc child -> Float.max acc (get child)) 0. children in
      (match node.kind with
      | RlColumn ->
          node.width <- largest (fun child -> child.width) +. sides pad;
          node.height <- sum (fun child -> child.height) +. gaps +. ends pad
      | _ ->
          node.width <- sum (fun child -> child.width) +. gaps +. sides pad;
          node.height <- largest (fun child -> child.height) +. ends pad);
      apply_explicit_size node

let offset_for align ~available ~used =
  match align with
  | AlignStart -> 0.
  | AlignCenter -> (available -. used) /. 2.
  | AlignEnd -> available -. used

let rec place fonts node ~x ~y =
  node.x <- x;
  node.y <- y;
  match node.kind with
  | RlLabel
  | RlButton
  | RlInput
  | RlSelect
  | RlText
  | RlBox
  | RlSpacer
  | RlImage
  | RlCheckbox
  | RlSlider
  | RlShortcut
  | RlTimer
  | RlCanvas
  | RlScene -> ()
  | RlTabs ->
      let header_height =
        number_style [ "tab-height" ] node.styles
          ~default:((font_size_of node.styles *. 1.2) +. (2. *. tab_header_padding))
      in
      let cursor = ref x in
      node.tab_rects <-
        Array.map
          (fun title ->
            let width =
              R.Vector2.x (text_extent fonts node.styles title) +. (2. *. tab_header_padding)
            in
            let rect = R.Rectangle.create !cursor y width header_height in
            cursor := !cursor +. width;
            rect)
          node.titles;
      List.iteri
        (fun index page ->
          if index = node.active then place fonts page ~x ~y:(y +. header_height))
        node.children
  | RlScroll ->
      let pad = padding_edges node.styles in
      let spacing = spacing_of node.styles in
      let align = Option.value (align_from_styles node.styles) ~default:AlignStart in
      let inner_width = node.width -. sides pad in
      (* The scroll offset is folded into the children's coordinates, so
         hit-testing sees exactly what the eye sees, with no second mapping. *)
      let cursor = ref (y +. pad.e_top -. node.scroll_y) in
      List.iter
        (fun child ->
          (* Filling the height means nothing here -- the box scrolls, so
             there is no fixed leftover to share. *)
          Option.iter
            (fun share -> child.width <- inner_width *. share)
            (percent_of [ "width" ] child.styles);
          if fills_width child then child.width <- inner_width;
          let dx = offset_for align ~available:inner_width ~used:child.width in
          place fonts child ~x:(x +. pad.e_left +. dx) ~y:!cursor;
          cursor := !cursor +. child.height +. spacing)
        (visible_children node)
  | RlColumn
  | RlRow ->
      let pad = padding_edges node.styles in
      let spacing = spacing_of node.styles in
      let align = Option.value (align_from_styles node.styles) ~default:AlignStart in
      let inner_width = node.width -. sides pad in
      let inner_height = node.height -. ends pad in
      let children = visible_children node in
      (* Percentages first: they do not depend on what is left over, and the
         fill share below has to see the sizes they settled on. *)
      List.iter
        (fun child ->
          Option.iter
            (fun share -> child.width <- inner_width *. share)
            (percent_of [ "width" ] child.styles);
          Option.iter
            (fun share -> child.height <- inner_height *. share)
            (percent_of [ "height" ] child.styles))
        children;
      (* Children asking to fill are resized before being placed, so that
         recursing into them lays their own contents out at the final size.
         Across the stacking axis the leftover room is shared between them;
         across the other one each simply takes the full width or height. *)
      let gaps =
        let count = List.length children in
        if count > 1 then spacing *. float_of_int (count - 1) else 0.
      in
      let share along_axis size_of =
        let flexible = List.filter along_axis children in
        match List.length flexible with
        | 0 -> 0.
        | count ->
            let taken =
              List.fold_left
                (fun acc child -> acc +. if along_axis child then 0. else size_of child)
                0. children
            in
            let room =
              (match node.kind with RlColumn -> inner_height | _ -> inner_width) -. taken -. gaps
            in
            Float.max 0. room /. float_of_int count
      in
      (match node.kind with
      | RlColumn ->
          let each = share fills_height (fun child -> child.height) in
          List.iter
            (fun child ->
              if fills_width child then child.width <- inner_width;
              if fills_height child then child.height <- each)
            children
      | _ ->
          let each = share fills_width (fun child -> child.width) in
          List.iter
            (fun child ->
              if fills_height child then child.height <- inner_height;
              if fills_width child then child.width <- each)
            children);
      let cursor = ref (match node.kind with RlColumn -> y +. pad.e_top | _ -> x +. pad.e_left) in
      List.iter
        (fun child ->
          match node.kind with
          | RlColumn ->
              let dx = offset_for align ~available:inner_width ~used:child.width in
              place fonts child ~x:(x +. pad.e_left +. dx) ~y:!cursor;
              cursor := !cursor +. child.height +. spacing
          | _ ->
              let dy = offset_for align ~available:inner_height ~used:child.height in
              place fonts child ~x:!cursor ~y:(y +. pad.e_top +. dy);
              cursor := !cursor +. child.width +. spacing)
        children

(* --- drawing -------------------------------------------------------------- *)

(* Declared here rather than with the other interaction state because the
   drawing pass highlights the thumb while it is held. *)
let dragging_scroll : rl_node option ref = ref None

let rect_of node = R.Rectangle.create node.x node.y node.width node.height

(* Where an open dropdown list goes. Below the box normally, above it when
   there is no room below and there is room above -- otherwise a select near
   the bottom edge would drop its entries off the window. Both the drawing and
   the hit-testing read this, so they cannot disagree. *)
let dropdown_rect node =
  let height = float_of_int (Array.length node.options) *. node.height in
  let below = node.y +. node.height in
  let screen = float_of_int (R.get_screen_height ()) in
  let y =
    if below +. height <= screen then below
    else if node.y -. height >= 0. then node.y -. height
    else below
  in
  R.Rectangle.create node.x y node.width height

(* The scrollbar, when the content actually overflows. *)
let scroll_metrics node =
  let overflow = node.content_height -. node.height in
  if overflow <= 0. then None
  else begin
    let thumb = Float.max 24. (node.height *. (node.height /. node.content_height)) in
    let travel = Float.max 1. (node.height -. thumb) in
    Some (overflow, thumb, travel)
  end

let scroll_thumb_rect node =
  match scroll_metrics node with
  | None -> None
  | Some (overflow, thumb, travel) ->
      let progress = node.scroll_y /. overflow in
      Some (R.Rectangle.create (node.x +. node.width -. 8.) (node.y +. (progress *. travel)) 5. thumb)

(* The pointer shape is the strongest hint that something is interactive --
   stronger than any colour change. Collected during the dispatch pass and
   applied once, so the topmost widget under the mouse decides. *)
let wanted_cursor = ref R.MouseCursor.Default

let roundness_of styles ~width ~height =
  match Option.bind (find_first_literal [ "border-radius"; "radius" ] styles) int_from_literal with
  | Some value when value > 0 ->
      let shortest = Float.min width height in
      if shortest <= 0. then 0. else Float.min 1. (float_of_int value *. 2. /. shortest)
  | _ -> 0.

let fill_rect ?(roundness = 0.) rect color =
  if roundness > 0. then R.draw_rectangle_rounded rect roundness 16 color
  else R.draw_rectangle_rec rect color

(* An open dropdown has to cover whatever is under it, so it is not drawn in
   tree order: the pass collects it here and replays it last. *)
let overlays : (unit -> unit) list ref = ref []

(* The caret blinks off a clock we advance ourselves; raylib has no timer we
   want to depend on here. *)
let blink_clock = ref 0.

let draw_centred_text fonts styles rect text color =
  let extent = text_extent fonts styles text in
  let x = R.Rectangle.x rect +. ((R.Rectangle.width rect -. R.Vector2.x extent) /. 2.) in
  let y = R.Rectangle.y rect +. ((R.Rectangle.height rect -. R.Vector2.y extent) /. 2.) in
  R.draw_text_ex (face fonts styles) text (R.Vector2.create x y) (font_size_of styles)
    glyph_spacing color

let draw_chevron rect color =
  let size = 5. in
  let cx = R.Rectangle.x rect +. R.Rectangle.width rect -. 18. in
  let cy = R.Rectangle.y rect +. (R.Rectangle.height rect /. 2.) -. 2. in
  R.draw_triangle
    (R.Vector2.create (cx -. size) (cy -. (size /. 2.)))
    (R.Vector2.create cx (cy +. (size /. 2.)))
    (R.Vector2.create (cx +. size) (cy -. (size /. 2.)))
    color

(* --- interaction ----------------------------------------------------------

   Same contract as the Bogue backend: the handler runs in a child of the
   environment captured when the window was built, with [event_widget] and
   [event_value] bound, and a failing handler reports without tearing the
   window down. *)

(* The widget it fired on and the value that came with it are passed to the
   handler, which binds them as 'event_widget' and 'event_value'. A failing
   handler reports and the window stays up: a mistake in one button should not
   take the program with it. *)
let run_callback ~node ?(value = V_null) handler =
  let arguments = [ make_object "widget" node; value ] in
  try
    match handler with
    | V_native fn -> ignore (fn arguments)
    | other -> prerr_endline ("GUI handler is not callable: " ^ value_to_string other)
  with
  | Return_signal _ -> ()
  | Runtime_error message -> prerr_endline ("GUI runtime error: " ^ message)
  | exn -> prerr_endline ("GUI callback failed: " ^ Printexc.to_string exn)

let fire node value pick =
  List.iter
    (fun event ->
      match pick event with
      | Some handler -> run_callback ~node ~value handler
      | None -> ())
    node.events

let on_click = function GuiOnClick block -> Some block | _ -> None
let on_hover = function GuiOnHover block -> Some block | _ -> None
let on_input = function GuiOnInput block -> Some block | _ -> None
let on_change = function GuiOnChange block -> Some block | _ -> None
let on_tick = function GuiOnTick block -> Some block | _ -> None
let on_draw = function GuiOnDraw block -> Some block | _ -> None

(* The canvas being painted right now, if any. The 'draw' verbs read it to know
   where their coordinates start and how big the surface is; outside a canvas
   there is nothing for them to draw on and they say so. *)
let current_canvas : rl_node option ref = ref None

(* --- the 3D scene ---------------------------------------------------------

   A Scene is a Canvas with a camera in front of it. What makes it look like
   anything is the shader below: raylib's own draws every face in a flat colour,
   so a cube reads as a hexagon and a sphere as a disc. This gives each fragment
   a normal and asks four lights what they think of it.

   Glass is not refraction, which would need the frame rendered to a texture and
   sampled back. It is transparency, drawn after everything solid and from the
   back forward so the blending stacks in the right order, with the edges lit
   more than the middle -- which is what the eye actually reads as glass. *)

let scene_vertex_shader =
  {glsl|#version 330
in vec3 vertexPosition;
in vec3 vertexNormal;
in vec4 vertexColor;
uniform mat4 mvp;
uniform mat4 matModel;
uniform mat4 matNormal;
out vec3 fragPosition;
out vec3 fragNormal;
out vec4 fragColor;
void main() {
  fragPosition = vec3(matModel * vec4(vertexPosition, 1.0));
  fragNormal = normalize(vec3(matNormal * vec4(vertexNormal, 1.0)));
  fragColor = vertexColor;
  gl_Position = mvp * vec4(vertexPosition, 1.0);
}|glsl}

let scene_fragment_shader =
  {glsl|#version 330
in vec3 fragPosition;
in vec3 fragNormal;
in vec4 fragColor;
uniform vec4 colDiffuse;
uniform vec3 viewPos;
uniform vec3 lightPos[4];
uniform vec3 lightColor[4];
uniform int lightCount;
uniform vec3 ambient;
uniform float shininess;
// 0 for a solid surface, 1 for glass. Glass keeps its own alpha and gains a
// rim: the more the surface turns away from the eye, the brighter its edge.
uniform float glassiness;
out vec4 finalColor;
void main() {
  vec4 base = fragColor * colDiffuse;
  vec3 normal = normalize(fragNormal);
  vec3 toEye = normalize(viewPos - fragPosition);
  if (dot(normal, toEye) < 0.0) normal = -normal;
  vec3 lit = ambient * base.rgb;
  for (int index = 0; index < lightCount; index++) {
    vec3 toLight = normalize(lightPos[index] - fragPosition);
    float diffuse = max(dot(normal, toLight), 0.0);
    vec3 halfway = normalize(toLight + toEye);
    float specular = pow(max(dot(normal, halfway), 0.0), shininess);
    lit += base.rgb * lightColor[index] * diffuse;
    lit += lightColor[index] * specular * 0.6;
  }
  float alpha = base.a;
  if (glassiness > 0.5) {
    float rim = 1.0 - max(dot(normal, toEye), 0.0);
    rim = pow(rim, 2.0);
    lit += base.rgb * rim * 1.6;
    alpha = clamp(alpha + rim * 0.5, 0.0, 1.0);
  }
  // Three lights adding up ran straight past white and everything pale came out
  // as a flat blob. This bends the top of the range back down instead of cutting
  // it off, so a bright surface keeps its colour rather than turning white.
  lit = lit / (lit + vec3(1.0)) * 1.9;
  finalColor = vec4(min(lit, vec3(1.0)), alpha);
}|glsl}

type scene_state = {
  mutable shader : R.Shader.t option;
  mutable camera : R.Camera3D.t option;
  (* Where each uniform lives in the shader, looked up once. *)
  mutable loc_view : R.ShaderLoc.t option;
  mutable loc_light_pos : R.ShaderLoc.t option;
  mutable loc_light_color : R.ShaderLoc.t option;
  mutable loc_light_count : R.ShaderLoc.t option;
  mutable loc_ambient : R.ShaderLoc.t option;
  mutable loc_shininess : R.ShaderLoc.t option;
  mutable loc_glassiness : R.ShaderLoc.t option;
  (* Lights and glass shapes gathered during a frame. The glass cannot be drawn
     as it arrives: it has to wait until the solids are down, then go from the
     furthest to the nearest. *)
  mutable lights : (float * float * float * float * float * float) list;
  mutable deferred : (float * (unit -> unit)) list;
  mutable inside : bool;
}

let scene =
  {
    shader = None;
    camera = None;
    loc_view = None;
    loc_light_pos = None;
    loc_light_color = None;
    loc_light_count = None;
    loc_ambient = None;
    loc_shininess = None;
    loc_glassiness = None;
    lights = [];
    deferred = [];
    inside = false;
  }

let scene_shader () =
  match scene.shader with
  | Some shader -> shader
  | None ->
      let shader = R.load_shader_from_memory scene_vertex_shader scene_fragment_shader in
      scene.shader <- Some shader;
      scene.loc_view <- Some (R.get_shader_location shader "viewPos");
      scene.loc_light_pos <- Some (R.get_shader_location shader "lightPos");
      scene.loc_light_color <- Some (R.get_shader_location shader "lightColor");
      scene.loc_light_count <- Some (R.get_shader_location shader "lightCount");
      scene.loc_ambient <- Some (R.get_shader_location shader "ambient");
      scene.loc_shininess <- Some (R.get_shader_location shader "shininess");
      scene.loc_glassiness <- Some (R.get_shader_location shader "glassiness");
      shader

(* set_shader_value takes a raw pointer, so each value has to be given somewhere
   in memory to live for the length of the call. *)
let uniform_float shader location value =
  match location with
  | Some loc ->
      let cell = Ctypes.allocate Ctypes.float value in
      R.set_shader_value shader loc (Ctypes.to_voidp cell) R.ShaderUniformDataType.Float
  | None -> ()

and uniform_int shader location value =
  match location with
  | Some loc ->
      let cell = Ctypes.allocate Ctypes.int value in
      R.set_shader_value shader loc (Ctypes.to_voidp cell) R.ShaderUniformDataType.Int
  | None -> ()

and uniform_vec3 shader location x y z =
  match location with
  | Some loc ->
      let values = Ctypes.CArray.of_list Ctypes.float [ x; y; z ] in
      R.set_shader_value shader loc
        (Ctypes.to_voidp (Ctypes.CArray.start values))
        R.ShaderUniformDataType.Vec3
  | None -> ()

and uniform_vec3_array shader location floats count =
  match location with
  | Some loc ->
      let values = Ctypes.CArray.of_list Ctypes.float floats in
      R.set_shader_value_v shader loc
        (Ctypes.to_voidp (Ctypes.CArray.start values))
        R.ShaderUniformDataType.Vec3 count
  | None -> ()

(* The typeface of the window being drawn. The renderer is handed it as an
   argument, but 'draw.text' is reached from the program's own handler, which has
   no way to be handed anything. *)
let active_fonts : fonts option ref = ref None

let rec render fonts ~focused node =
  if node.visible then
    match node.kind with
    | RlShortcut
    | RlTimer -> ()
    | RlSpacer -> (
        match background_of node.styles with
        | Some color -> fill_rect (rect_of node) color
        | None -> ())
    | RlBox ->
        fill_rect (rect_of node)
          (color_or panel_color [ "background"; "background-color"; "color" ] node.styles)
          ~roundness:(roundness_of node.styles ~width:node.width ~height:node.height)
    (* The program paints this one. Its background is filled first, then its
       onDraw handler runs -- here, inside the frame, which is what lets the
       'draw' verbs put shapes on the screen rather than into a list. Everything
       it draws is clipped to the canvas and placed relative to its top left, so
       a program can work in its own coordinates and cannot scribble over the
       widgets around it. *)
    | RlCanvas ->
        (match background_of node.styles with
        | Some color -> fill_rect (rect_of node) color
        | None -> ());
        let previous = !current_canvas in
        current_canvas := Some node;
        R.begin_scissor_mode
          (int_of_float node.x) (int_of_float node.y)
          (int_of_float node.width) (int_of_float node.height);
        fire node V_null on_draw;
        R.end_scissor_mode ();
        current_canvas := previous
    (* Two passes over the same handler's output. The handler runs once and says
       what is in the scene; the solids are drawn as they are named, the glass is
       set aside and drawn afterwards, furthest first. *)
    | RlScene ->
        (match background_of node.styles with
        | Some color -> fill_rect (rect_of node) color
        | None -> ());
        let camera =
          match scene.camera with
          | Some camera -> camera
          | None ->
              let camera =
                R.Camera3D.create
                  (R.Vector3.create 6. 5. 6.)
                  (R.Vector3.create 0. 0. 0.)
                  (R.Vector3.create 0. 1. 0.)
                  45. R.CameraProjection.Perspective
              in
              scene.camera <- Some camera;
              camera
        in
        let previous = !current_canvas in
        current_canvas := Some node;
        scene.lights <- [];
        scene.deferred <- [];
        scene.inside <- true;
        R.begin_scissor_mode
          (int_of_float node.x) (int_of_float node.y)
          (int_of_float node.width) (int_of_float node.height);
        R.begin_mode_3d camera;
        let shader = scene_shader () in
        R.begin_shader_mode shader;
        fire node V_null on_draw;
        (* The lights were only collected while the handler ran, so the uniforms
           can only be set now -- which is fine, since nothing has reached the
           screen yet: raylib batches until the mode ends. *)
        let lights = List.rev scene.lights in
        let count = min 4 (List.length lights) in
        let positions = Array.make 12 0. and colours = Array.make 12 0. in
        List.iteri
          (fun index (x, y, z, r, g, b) ->
            if index < 4 then begin
              positions.(index * 3) <- x;
              positions.((index * 3) + 1) <- y;
              positions.((index * 3) + 2) <- z;
              colours.(index * 3) <- r;
              colours.((index * 3) + 1) <- g;
              colours.((index * 3) + 2) <- b
            end)
          lights;
        let eye = R.Camera3D.position camera in
        uniform_vec3 shader scene.loc_view (R.Vector3.x eye) (R.Vector3.y eye) (R.Vector3.z eye);
        uniform_vec3_array shader scene.loc_light_pos (Array.to_list positions) 4;
        uniform_vec3_array shader scene.loc_light_color (Array.to_list colours) 4;
        uniform_int shader scene.loc_light_count count;
        R.end_shader_mode ();
        R.end_mode_3d ();
        (* Now the glass, in a second 3D pass so that depth writing can be off:
           a transparent face must not hide the one behind it. *)
        if scene.deferred <> [] then begin
          R.begin_mode_3d camera;
          R.begin_shader_mode shader;
          R.begin_blend_mode R.BlendMode.Alpha;
          scene.deferred
          |> List.sort (fun (a, _) (b, _) -> compare b a)
          |> List.iter (fun (_, draw) -> draw ());
          R.end_blend_mode ();
          R.end_shader_mode ();
          R.end_mode_3d ()
        end;
        scene.inside <- false;
        R.end_scissor_mode ();
        current_canvas := previous
    | RlImage -> (
        match node.texture with
        | Some texture ->
            R.draw_texture_pro texture
              (R.Rectangle.create 0. 0.
                 (float_of_int (R.Texture.width texture))
                 (float_of_int (R.Texture.height texture)))
              (rect_of node)
              (R.Vector2.create 0. 0.) 0. (R.Color.create 255 255 255 255)
        | None ->
            (* The file was missing or unreadable: show an outline rather than
               nothing, so the gap is visible during development. *)
            R.draw_rectangle_lines_ex (rect_of node) 1. muted_color)
    | RlText ->
        let step = line_height fonts node.styles in
        let colour = text_color_of node.styles in
        let font = face fonts node.styles in
        let size = font_size_of node.styles in
        let align = Option.value (text_align_from_styles node.styles) ~default:AlignStart in
        List.iteri
          (fun index line ->
            let used = R.Vector2.x (text_extent fonts node.styles line) in
            let dx = offset_for align ~available:node.width ~used in
            R.draw_text_ex font line
              (R.Vector2.create (node.x +. Float.max 0. dx) (node.y +. (step *. float_of_int index)))
              size glyph_spacing colour)
          node.lines
    | RlCheckbox ->
        let size = checkbox_size_of node.styles in
        let box =
          R.Rectangle.create node.x (node.y +. ((node.height -. size) /. 2.)) size size
        in
        let base = Option.value (background_of node.styles) ~default:panel_color in
        let fill = if node.checked then focus_color_of node.styles else base in
        fill_rect box
          (dim node.enabled (if node.hovered then hover_fill node.styles fill else fill))
          ~roundness:0.3;
        if not node.checked then R.draw_rectangle_lines_ex box 1. (border_color_of node.styles);
        if node.checked then begin
          (* The tick is expressed as fractions of the box so that changing
             'size' scales it instead of leaving a mark the wrong shape. *)
          let x = R.Rectangle.x box and y = R.Rectangle.y box in
          let at fx fy = R.Vector2.create (x +. (size *. fx)) (y +. (size *. fy)) in
          let mark = color_or (R.Color.create 255 255 255 255) [ "tick-color" ] node.styles in
          R.draw_line_ex (at 0.23 0.50) (at 0.41 0.68) (size /. 10.) mark;
          R.draw_line_ex (at 0.41 0.68) (at 0.77 0.32) (size /. 10.) mark
        end;
        (* Bogue's checkbox has no label; drawing one when the script supplies
           text is strictly more useful and breaks nothing. *)
        if not (String.equal node.text "") then begin
          let extent = text_extent fonts node.styles node.text in
          R.draw_text_ex (face fonts node.styles) node.text
            (R.Vector2.create
               (node.x +. size +. checkbox_gap)
               (node.y +. ((node.height -. R.Vector2.y extent) /. 2.)))
            (font_size_of node.styles) glyph_spacing (text_color_of node.styles)
        end
    | RlSlider ->
        let track_height = number_style [ "track-height" ] node.styles ~default:6. in
        let middle = node.y +. (node.height /. 2.) in
        let track =
          R.Rectangle.create node.x (middle -. (track_height /. 2.)) node.width track_height
        in
        fill_rect track (shift_color (-10) panel_color) ~roundness:1.;
        let progress =
          if node.slider_max <= 0. then 0. else node.slider_value /. node.slider_max
        in
        fill_rect
          (R.Rectangle.create node.x (middle -. (track_height /. 2.)) (node.width *. progress)
             track_height)
          (dim node.enabled (Option.value (background_of node.styles) ~default:accent_color))
          ~roundness:1.;
        let knob = number_style [ "knob-size" ] node.styles ~default:9. in
        let knob_radius = if node.hovered || node.pressed then knob +. 2. else knob in
        R.draw_circle_v
          (R.Vector2.create (node.x +. (node.width *. progress)) middle)
          knob_radius
          (dim node.enabled (Option.value (background_of node.styles) ~default:accent_color))
    | RlLabel ->
        (* text-align only has room to act when the label was given a width
           wider than its text; otherwise the box is the text. *)
        let extent = text_extent fonts node.styles node.text in
        let align = Option.value (text_align_from_styles node.styles) ~default:AlignStart in
        let dx = offset_for align ~available:node.width ~used:(R.Vector2.x extent) in
        R.draw_text_ex (face fonts node.styles) node.text
          (R.Vector2.create (node.x +. Float.max 0. dx) node.y)
          (font_size_of node.styles) glyph_spacing (text_color_of node.styles)
    | RlButton ->
        let base = Option.value (background_of node.styles) ~default:accent_color in
        let color =
          if node.pressed && node.hovered then active_fill node.styles base
          else if node.hovered then hover_fill node.styles base
          else base
        in
        let rect = rect_of node in
        fill_rect rect (dim node.enabled color)
          ~roundness:(roundness_of node.styles ~width:node.width ~height:node.height);
        draw_centred_text fonts node.styles rect node.text
          (dim node.enabled
             (color_or (R.Color.create 255 255 255 255) [ "color"; "text-color" ] node.styles))
    | RlInput ->
        let rect = rect_of node in
        let roundness = roundness_of node.styles ~width:node.width ~height:node.height in
        let has_focus = match focused with Some target -> target == node | None -> false in
        let base = Option.value (background_of node.styles) ~default:panel_color in
        fill_rect rect
          (dim node.enabled (if node.hovered then hover_fill node.styles base else base))
          ~roundness;
        (* Focus wins over hover: an accent outline when it has the keyboard,
           a discreet one when the mouse is merely passing over. *)
        if has_focus then
          R.draw_rectangle_rounded_lines rect roundness 16 (focus_color_of node.styles)
        else if node.hovered then
          R.draw_rectangle_rounded_lines rect roundness 16 (border_color_of node.styles);
        let padding = padding_of node.styles in
        let font = face fonts node.styles in
        let size = font_size_of node.styles in
        let showing_placeholder = String.equal node.text "" && not has_focus in
        let shown = if showing_placeholder then node.placeholder else node.text in
        let colour = if showing_placeholder then muted_color else text_color_of node.styles in
        let text_y = node.y +. ((node.height -. R.Vector2.y (text_extent fonts node.styles "A")) /. 2.) in
        let inner_width = node.width -. (2. *. padding) in
        (* Keep the caret inside the visible part. Done here rather than at
           every edit because the caret also moves by mouse and by arrow key,
           and this runs after all of them. *)
        let caret_width = prefix_width fonts node.styles node.text node.caret in
        if caret_width -. node.text_offset > inner_width then
          node.text_offset <- caret_width -. inner_width;
        if caret_width -. node.text_offset < 0. then node.text_offset <- caret_width;
        node.text_offset <- Float.max 0. node.text_offset;
        let origin = node.x +. padding -. node.text_offset in
        (* Long text is cut at the box edge rather than spilling over its
           neighbours. *)
        R.begin_scissor_mode
          (int_of_float (node.x +. padding))
          (int_of_float node.y)
          (int_of_float inner_width)
          (int_of_float node.height);
        if has_focus && has_selection node then begin
          let first, last = selection_range node in
          let from_x = origin +. prefix_width fonts node.styles node.text first in
          let to_x = origin +. prefix_width fonts node.styles node.text last in
          R.draw_rectangle
            (int_of_float from_x)
            (int_of_float (text_y -. 2.))
            (int_of_float (to_x -. from_x))
            (int_of_float (size +. 6.))
            (Option.value
               (color_style [ "selection-color" ] node.styles)
               ~default:(R.Color.create 59 130 246 120))
        end;
        R.draw_text_ex font shown (R.Vector2.create origin text_y) size glyph_spacing colour;
        (* The caret hides while a selection is showing, as everywhere else. *)
        if has_focus && (not (has_selection node)) && Float.rem !blink_clock 1.0 < 0.5 then
          R.draw_rectangle
            (int_of_float (origin +. caret_width))
            (int_of_float (text_y -. 2.))
            2
            (int_of_float (size +. 4.))
            default_text_color;
        R.end_scissor_mode ()
    | RlSelect ->
        let rect = rect_of node in
        let roundness = roundness_of node.styles ~width:node.width ~height:node.height in
        let base = Option.value (background_of node.styles) ~default:panel_color in
        fill_rect rect
          (dim node.enabled (if node.hovered then hover_fill node.styles base else base))
          ~roundness;
        if node.hovered || node.expanded then
          R.draw_rectangle_rounded_lines rect roundness 16
            (if node.expanded then focus_color_of node.styles else border_color_of node.styles);
        let label =
          if Array.length node.options = 0 then ""
          else node.options.(min node.selected (Array.length node.options - 1))
        in
        let padding = padding_of node.styles in
        let text_y =
          node.y +. ((node.height -. R.Vector2.y (text_extent fonts node.styles "A")) /. 2.)
        in
        R.draw_text_ex (face fonts node.styles) label
          (R.Vector2.create (node.x +. padding) text_y)
          (font_size_of node.styles) glyph_spacing (text_color_of node.styles);
        draw_chevron rect muted_color;
        if node.expanded then
          overlays :=
            !overlays
            @ [
                (fun () ->
                  let list_rect = dropdown_rect node in
                  Array.iteri
                    (fun index option ->
                      let item =
                        R.Rectangle.create node.x
                          (R.Rectangle.y list_rect +. (float_of_int index *. node.height))
                          node.width node.height
                      in
                      fill_rect item
                        (if index = node.selected then shift_color 22 base else base);
                      let text_y =
                        R.Rectangle.y item
                        +. ((node.height -. R.Vector2.y (text_extent fonts node.styles "A")) /. 2.)
                      in
                      R.draw_text_ex (face fonts node.styles) option
                        (R.Vector2.create (node.x +. padding) text_y)
                        (font_size_of node.styles) glyph_spacing (text_color_of node.styles))
                    node.options);
              ]
    | RlTabs ->
        Array.iteri
          (fun index rect ->
            let selected = index = node.active in
            let base = if selected then panel_color else shift_color (-8) panel_color in
            fill_rect rect
              (if (not selected) && index = node.hovered_tab then hover_fill node.styles base
               else base);
            draw_centred_text fonts node.styles rect node.titles.(index)
              (if selected || index = node.hovered_tab then default_text_color else muted_color);
            if selected then
              R.draw_rectangle
                (int_of_float (R.Rectangle.x rect))
                (int_of_float (R.Rectangle.y rect +. R.Rectangle.height rect -. 3.))
                (int_of_float (R.Rectangle.width rect))
                3 accent_color)
          node.tab_rects;
        List.iteri
          (fun index page -> if index = node.active then render fonts ~focused page)
          node.children
    | RlScroll ->
        let rect = rect_of node in
        (match background_of node.styles with
        | Some color ->
            fill_rect rect color
              ~roundness:(roundness_of node.styles ~width:node.width ~height:node.height)
        | None -> ());
        (* Everything past the box is cut away rather than drawn over the
           rest of the window. *)
        R.begin_scissor_mode (int_of_float node.x) (int_of_float node.y)
          (int_of_float node.width) (int_of_float node.height);
        List.iter (render fonts ~focused) node.children;
        R.end_scissor_mode ();
        (match scroll_thumb_rect node with
        | Some thumb ->
            R.draw_rectangle_rounded thumb 1. 8
              (if is_set dragging_scroll then default_text_color else muted_color)
        | None -> ())
    | RlColumn
    | RlRow ->
        (match background_of node.styles with
        | Some color ->
            fill_rect (rect_of node) color
              ~roundness:(roundness_of node.styles ~width:node.width ~height:node.height)
        | None -> ());
        List.iter (render fonts ~focused) node.children

let inside rect point = R.check_collision_point_rec point rect

(* Which node has the keyboard, and which dropdown is covering the window.
   Both are per-window, reset when a window opens. *)
let focused : rl_node option ref = ref None
let expanded_select : rl_node option ref = ref None

(* Set while the mouse is held down inside a field, so moving the pointer
   extends the selection instead of hovering over the widgets it crosses. *)
let dragging_input : rl_node option ref = ref None

(* Same idea for a slider: once grabbed, it keeps following the mouse even if
   the pointer wanders off the track. *)
let dragging_slider : rl_node option ref = ref None

(* Monotonic, in seconds since the window opened. Only used to tell a
   double-click from two separate clicks. *)
let wall_clock = ref 0.

(* raylib spins at the target frame rate whether or not anything moved, which
   on a form that just sits there is a fan running for nothing. Blocking on
   the next event drops that to zero -- except while something genuinely
   animates, which for us means a blinking caret or a drag in progress. *)
let event_waiting = ref false

let set_event_waiting want =
  if want <> !event_waiting then begin
    event_waiting := want;
    if want then R.enable_event_waiting () else R.disable_event_waiting ()
  end

let modifier_down = function
  | ModCtrl -> R.is_key_down R.Key.Left_control || R.is_key_down R.Key.Right_control
  | ModShift -> R.is_key_down R.Key.Left_shift || R.is_key_down R.Key.Right_shift
  | ModAlt -> R.is_key_down R.Key.Left_alt || R.is_key_down R.Key.Right_alt

let unfocus () = focused := None

let collapse_select () =
  match !expanded_select with
  | Some node ->
      node.expanded <- false;
      expanded_select := None
  | None -> ()

(* --- text editing ---------------------------------------------------------

   Follows the raylib text-input example for reading the codepoint queue, but
   every edit moves the caret over whole UTF-8 sequences. No selection yet, so
   copy takes the whole field -- stated plainly rather than half-implemented. *)

let selected_text node =
  let first, last = selection_range node in
  String.sub node.text first (last - first)

let delete_selection node =
  if not (has_selection node) then false
  else begin
    let first, last = selection_range node in
    node.text <-
      String.sub node.text 0 first ^ String.sub node.text last (String.length node.text - last);
    node.caret <- first;
    node.anchor <- first;
    true
  end

(* Typing over a selection replaces it, as everywhere else. *)
let insert_text node addition =
  ignore (delete_selection node);
  let before = String.sub node.text 0 node.caret in
  let after = String.sub node.text node.caret (String.length node.text - node.caret) in
  node.text <- before ^ addition ^ after;
  node.caret <- node.caret + String.length addition;
  node.anchor <- node.caret

let edit_focused_input node =
  let changed = ref false in
  (* The queue can hold several codepoints for one frame. *)
  let rec drain () =
    let uchar = R.get_char_pressed () in
    if Uchar.to_int uchar <> 0 then begin
      insert_text node (utf8_of_uchar uchar);
      changed := true;
      drain ()
    end
  in
  drain ();
  let pressed key = R.is_key_pressed key || R.is_key_pressed_repeat key in
  let control_down = R.is_key_down R.Key.Left_control || R.is_key_down R.Key.Right_control in
  let shift_down = R.is_key_down R.Key.Left_shift || R.is_key_down R.Key.Right_shift in
  (* Shift keeps the anchor where it is, so the selection grows; without it
     the anchor follows and the selection collapses. *)
  let move_to index =
    node.caret <- index;
    if not shift_down then node.anchor <- index;
    blink_clock := 0.
  in
  if pressed R.Key.Backspace then begin
    if delete_selection node then changed := true
    else if node.caret > 0 then begin
      let start = utf8_prev node.text node.caret in
      node.text <-
        String.sub node.text 0 start
        ^ String.sub node.text node.caret (String.length node.text - node.caret);
      node.caret <- start;
      node.anchor <- start;
      changed := true
    end
  end;
  if pressed R.Key.Delete then begin
    if delete_selection node then changed := true
    else if node.caret < String.length node.text then begin
      let stop = utf8_next node.text node.caret in
      node.text <-
        String.sub node.text 0 node.caret
        ^ String.sub node.text stop (String.length node.text - stop);
      changed := true
    end
  end;
  (* An unshifted arrow on a selection jumps to its edge rather than moving
     one character from the caret. *)
  if pressed R.Key.Left then
    if has_selection node && not shift_down then move_to (fst (selection_range node))
    else move_to (utf8_prev node.text node.caret);
  if pressed R.Key.Right then
    if has_selection node && not shift_down then move_to (snd (selection_range node))
    else move_to (utf8_next node.text node.caret);
  if pressed R.Key.Home then move_to 0;
  if pressed R.Key.End then move_to (String.length node.text);
  if control_down && R.is_key_pressed R.Key.A then begin
    node.anchor <- 0;
    node.caret <- String.length node.text
  end;
  if control_down && R.is_key_pressed R.Key.V then
    Option.iter
      (fun clip ->
        insert_text node clip;
        changed := true)
      (R.get_clipboard_text ());
  (* With nothing selected, copy takes the whole field -- the useful reading
     of Ctrl+C on a one-line input. *)
  if control_down && R.is_key_pressed R.Key.C then
    R.set_clipboard_text (if has_selection node then selected_text node else node.text);
  if control_down && R.is_key_pressed R.Key.X && has_selection node then begin
    R.set_clipboard_text (selected_text node);
    ignore (delete_selection node);
    changed := true
  end;
  if !changed then begin
    blink_clock := 0.;
    fire node (V_string node.text) on_input
  end;
  if R.is_key_pressed R.Key.Enter then
    fire node (V_string node.text) on_change

(* A click is press and release on the same widget -- pressing a control then
   sliding off before letting go must not fire it. [clip] is the visible part
   of the enclosing scroll box, so a widget scrolled out of sight cannot be
   clicked even though its rectangle still exists. *)
let rec dispatch ~fonts ~mouse ~pressed_now ~released ~clip ~enabled node =
  if node.visible then begin
    (* A disabled subtree still draws, but nothing in it answers the mouse. *)
    let enabled = enabled && node.enabled in
    let reachable rect =
      enabled && inside rect mouse
      && match clip with Some area -> inside area mouse | None -> true
    in
    (match node.kind with
    | RlButton ->
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over then wanted_cursor := R.MouseCursor.Pointing_hand;
        if over && pressed_now then node.pressed <- true;
        if released then begin
          if over && node.pressed then fire node V_null on_click;
          node.pressed <- false
        end
    | RlInput ->
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over then wanted_cursor := R.MouseCursor.Ibeam;
        (* Pixel position inside the text, undoing both the padding and the
           sideways scroll. *)
        let local_x () =
          R.Vector2.x mouse -. (node.x +. padding_of node.styles) +. node.text_offset
        in
        if pressed_now && over then begin
          focused := Some node;
          let index = index_at_x fonts node (local_x ()) in
          (* A second click on the same field within 400 ms takes the word
             under the caret, as every other text field does. *)
          if !wall_clock -. node.last_click_at < 0.4 then begin
            let first, last = word_bounds node.text index in
            node.anchor <- first;
            node.caret <- last
          end
          else begin
            node.caret <- index;
            node.anchor <- index;
            dragging_input := Some node
          end;
          node.last_click_at <- !wall_clock;
          blink_clock := 0.
        end;
        (match !dragging_input with
        | Some target when target == node ->
            if R.is_mouse_button_down R.MouseButton.Left then
              node.caret <- index_at_x fonts node (local_x ())
            else dragging_input := None
        | _ -> ())
    | RlSelect ->
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over then wanted_cursor := R.MouseCursor.Pointing_hand;
        if pressed_now && over && not node.expanded then begin
          collapse_select ();
          node.expanded <- true;
          expanded_select := Some node
        end
    | RlTabs ->
        node.hovered_tab <- -1;
        Array.iteri
          (fun index rect ->
            if reachable rect then begin
              node.hovered_tab <- index;
              if index <> node.active then wanted_cursor := R.MouseCursor.Pointing_hand;
              if pressed_now then node.active <- index
            end)
          node.tab_rects
    | RlScroll ->
        let overflow = Float.max 0. (node.content_height -. node.height) in
        let scroll_by amount =
          node.scroll_y <- Float.max 0. (Float.min overflow (node.scroll_y +. amount))
        in
        let over = reachable (rect_of node) in
        if over then begin
          let wheel = R.get_mouse_wheel_move () in
          if Float.abs wheel > 0. then scroll_by (-.wheel *. 40.);
          (* Keys only when nothing is being typed into, otherwise a field
             inside the box could not use its own arrows. *)
          if not (is_set focused) then begin
            if R.is_key_pressed R.Key.Page_down || R.is_key_pressed_repeat R.Key.Page_down then
              scroll_by node.height;
            if R.is_key_pressed R.Key.Page_up || R.is_key_pressed_repeat R.Key.Page_up then
              scroll_by (-.node.height);
            if R.is_key_pressed R.Key.Down || R.is_key_pressed_repeat R.Key.Down then
              scroll_by 40.;
            if R.is_key_pressed R.Key.Up || R.is_key_pressed_repeat R.Key.Up then scroll_by (-40.);
            if R.is_key_pressed R.Key.Home then node.scroll_y <- 0.;
            if R.is_key_pressed R.Key.End then node.scroll_y <- overflow
          end
        end;
        (* Dragging the thumb. Grabbing the bar anywhere jumps to that spot,
           then keeps following even if the pointer leaves the bar. *)
        (match scroll_metrics node with
        | None -> ()
        | Some (overflow, thumb, travel) ->
            let bar =
              R.Rectangle.create (node.x +. node.width -. 12.) node.y 12. node.height
            in
            let follow () =
              let wanted = R.Vector2.y mouse -. node.y -. (thumb /. 2.) in
              node.scroll_y <-
                Float.max 0. (Float.min overflow (wanted /. travel *. overflow))
            in
            if pressed_now && inside bar mouse then begin
              dragging_scroll := Some node;
              follow ()
            end;
            (match !dragging_scroll with
            | Some target when target == node ->
                if R.is_mouse_button_down R.MouseButton.Left then follow ()
                else dragging_scroll := None
            | _ -> ()))
    | RlCheckbox ->
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over then wanted_cursor := R.MouseCursor.Pointing_hand;
        if over && pressed_now then node.pressed <- true;
        if released then begin
          if over && node.pressed then begin
            node.checked <- not node.checked;
            fire node (V_bool node.checked) on_change;
            fire node (V_bool node.checked) on_click
          end;
          node.pressed <- false
        end
    | RlSlider ->
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over then wanted_cursor := R.MouseCursor.Pointing_hand;
        let set_from_mouse () =
          let ratio =
            if node.width <= 0. then 0.
            else Float.max 0. (Float.min 1. ((R.Vector2.x mouse -. node.x) /. node.width))
          in
          let raw = ratio *. node.slider_max in
          let snapped =
            match node.slider_step with
            | Some step when step > 0. -> Float.round (raw /. step) *. step
            | _ -> raw
          in
          let clamped = Float.max 0. (Float.min node.slider_max snapped) in
          if clamped <> node.slider_value then begin
            node.slider_value <- clamped;
            fire node (V_int (int_of_float clamped)) on_change
          end
        in
        if over && pressed_now then begin
          node.pressed <- true;
          dragging_slider := Some node;
          set_from_mouse ()
        end;
        (match !dragging_slider with
        | Some target when target == node ->
            if R.is_mouse_button_down R.MouseButton.Left then set_from_mouse ()
            else begin
              dragging_slider := None;
              node.pressed <- false
            end
        | _ -> ())
    | RlTimer ->
        (* Fires on its own, with no user action at all -- which is why the
           loop must not block on events while one exists. The next deadline
           is taken from now rather than from the last one, so a stall does
           not produce a burst of catch-up ticks. *)
        if enabled && !wall_clock -. node.last_tick >= node.interval then begin
          node.last_tick <- !wall_clock;
          fire node (V_float !wall_clock) on_tick
        end
    | RlShortcut -> (
        match node.shortcut with
        | None -> ()
        | Some (modifiers, key) ->
            (* A shortcut with no modifier would otherwise fire on every
               keystroke aimed at a focused field. *)
            let typing = modifiers = [] && !focused <> None in
            if (not typing) && List.for_all modifier_down modifiers && R.is_key_pressed key then
              fire node V_null on_click)
    | RlBox ->
        (* A Box already accepted handlers; without this they were stored and
           never fired, which is the silent no-op the language went out of its
           way to remove elsewhere. A clickable rectangle is a card. *)
        let over = reachable (rect_of node) in
        if over <> node.hovered then begin
          node.hovered <- over;
          fire node (V_bool over) on_hover
        end;
        if over && node.events <> [] then wanted_cursor := R.MouseCursor.Pointing_hand;
        if over && pressed_now then node.pressed <- true;
        if released then begin
          if over && node.pressed then fire node V_null on_click;
          node.pressed <- false
        end
    | RlColumn
    | RlRow
    | RlLabel
    | RlText
    | RlSpacer
    (* A canvas takes no input of its own: it paints, and anything it should
       react to is a widget beside it. *)
    | RlCanvas
    | RlScene
    | RlImage -> ());
    (* Children of a scroll box inherit its rectangle as their clip. Tabs only
       hand events to the page on show. *)
    let child_clip = if node.kind = RlScroll then Some (rect_of node) else clip in
    match node.kind with
    | RlTabs ->
        List.iteri
          (fun index page ->
            if index = node.active then
              dispatch ~fonts ~mouse ~pressed_now ~released ~clip:child_clip ~enabled page)
          node.children
    | _ ->
        List.iter
          (dispatch ~fonts ~mouse ~pressed_now ~released ~clip:child_clip ~enabled)
          node.children
  end

(* The open dropdown covers the window, so it takes the click before the tree
   underneath ever sees it. *)
let dispatch_expanded ~mouse ~pressed_now =
  match !expanded_select with
  | None -> false
  | Some node ->
      if not pressed_now then false
      else begin
        let count = Array.length node.options in
        let list_rect = dropdown_rect node in
        if inside list_rect mouse then begin
          let index =
            int_of_float ((R.Vector2.y mouse -. R.Rectangle.y list_rect) /. node.height)
          in
          if index >= 0 && index < count then begin
            node.selected <- index;
            collapse_select ();
            fire node (V_string node.options.(index)) on_change
          end;
          true
        end
        else begin
          (* A click anywhere else just closes it, and is not passed on. *)
          collapse_select ();
          true
        end
      end

(* A window holding a timer can never sleep on events: nothing would wake it,
   and the timer would stop the moment the window lost focus. Answered once at
   startup rather than walked every frame. *)
let rec holds_timer node =
  node.kind = RlTimer || List.exists holds_timer node.children

(* Textures need a GL context, so images are loaded once the window exists
   rather than while the tree is being built. A missing file leaves the node
   without a texture, and the drawing pass shows an outline instead. *)
let rec load_textures node =
  (match (node.kind, node.texture) with
  | RlImage, None when not (String.equal node.text "") ->
      if Sys.file_exists node.text then begin
        let texture = R.load_texture node.text in
        if R.Texture.width texture > 0 then begin
          R.set_texture_filter texture R.TextureFilter.Bilinear;
          node.texture <- Some texture
        end
      end
      else prerr_endline ("GUI warning: image not found: " ^ node.text)
  | _ -> ());
  List.iter load_textures node.children

(* --- widget handles exposed to the language ------------------------------- *)

let rec exports node acc =
  let acc =
    match node.id with
    | Some id -> (id, make_object "control" node) :: acc
    | None -> acc
  in
  List.fold_left (fun acc child -> exports child acc) acc node.children

(* Handles exported by id carry kind "control"; the one bound to
   [event_widget] inside a handler carries "widget", as in the Bogue backend.
   Both wrap an rl_node, so both are accepted here. *)
let node_of_value name = function
  | V_object object_value
    when String.equal object_value.kind "control" || String.equal object_value.kind "widget" ->
      (Obj.obj object_value.payload : rl_node)
  | value -> expect_object name "widget" value

(* --- running a window ----------------------------------------------------- *)

(* The whole of running a window, given a tree whose handlers are already
   callable and somewhere to publish the widgets it names. Both the evaluator and
   a compiled program end up here; they differ only in how they got the handlers
   and where the ids go. *)
let run_window ~publish ~should_run root_block =
  match convert_gui_root root_block with
  | GuiWindow window ->
      let styles = modern_window_styles (checked "Window" window.styles) in
      let root =
        make_node
          ~kind:(match window.orientation with OrientationRow -> RlRow | _ -> RlColumn)
          ~id:window.id ~styles
          (List.map (convert ~inherited:(inheritable styles)) window.children)
      in
      List.iter (fun (name, value) -> publish name value) (exports root []);
      if not should_run then V_null
      else begin
        (* Resizable because an application should fit the room it is given,
           not the size it happened to open at. MSAA smooths the rounded
           corners at the same time. *)
        R.set_config_flags R.ConfigFlags.(window_resizable + msaa_4x_hint);
        let requested_width = Option.value window.width ~default:800 in
        let requested_height = Option.value window.height ~default:600 in
        R.init_window requested_width requested_height window.title;
        (* The size in the header is a floor, not just a starting point: the
           window grows freely and the layout follows, but it never goes below
           what the program asked for. Without this the window manager lets it
           shrink past its own contents, which then get cut off. *)
        R.set_window_min_size requested_width requested_height;
        (* raylib only warns when it cannot get a GL context, then crashes on
           the first draw call. A broken or half-upgraded driver is a normal
           thing to hit on a user's machine, so it gets a message. *)
        if not (R.is_window_ready ()) then
          raise
            (Runtime_error
               "Could not open a window: no usable OpenGL 3.3 context. The graphics driver may \
                be missing, or updated but not yet reloaded (try rebooting).");
        R.set_target_fps 60;
        focused := None;
        expanded_select := None;
        dragging_input := None;
        dragging_slider := None;
        dragging_scroll := None;
        event_waiting := false;
        wall_clock := 0.;
        load_textures root;
        let ticking = holds_timer root in
        let fonts =
          {
            regular = load_face Fonts_data.regular regular_candidates;
            bold = load_face Fonts_data.bold bold_candidates;
          }
        in
        active_fonts := Some fonts;
        let background = Option.value (background_of styles) ~default:(R.Color.create 17 24 39 255) in
        (try
           while not (R.window_should_close ()) do
             let elapsed = R.get_frame_time () in
             blink_clock := !blink_clock +. elapsed;
             wall_clock := !wall_clock +. elapsed;
             measure fonts root;
             (* measure sizes the root to its content; the root is the window,
                so it takes the window's size instead. Without this, 'align:
                center' would centre children inside the content box rather
                than inside the window, and read as left-aligned. Re-read
                every frame so a resize is picked up. *)
             root.width <- float_of_int (R.get_screen_width ());
             root.height <- float_of_int (R.get_screen_height ());
             place fonts root ~x:0. ~y:0.;
             let mouse = R.get_mouse_position () in
             let pressed_now = R.is_mouse_button_pressed R.MouseButton.Left in
             let released = R.is_mouse_button_released R.MouseButton.Left in
             (* The dropdown eats the click if it wants it; otherwise a press
                anywhere drops keyboard focus before the tree reassigns it. *)
             wanted_cursor := R.MouseCursor.Default;
             let swallowed = dispatch_expanded ~mouse ~pressed_now in
             if not swallowed then begin
               if pressed_now then unfocus ();
               dispatch ~fonts ~mouse ~pressed_now ~released ~clip:None ~enabled:true root
             end;
             (* An open dropdown covers the window, so the pointer stays a
                hand over it whatever is underneath. *)
             if is_set expanded_select then wanted_cursor := R.MouseCursor.Pointing_hand;
             R.set_mouse_cursor !wanted_cursor;
             (* Anything that has to redraw without the user touching
                anything keeps the loop spinning; otherwise it sleeps. *)
             let animating =
               ticking || is_set focused || is_set dragging_input || is_set dragging_slider
               || is_set dragging_scroll
             in
             set_event_waiting (not animating);
             (match !focused with
             | Some node when node.visible -> edit_focused_input node
             | Some _ -> unfocus ()
             | None -> ());
             R.begin_drawing ();
             R.clear_background background;
             overlays := [];
             render fonts ~focused:!focused root;
             List.iter (fun draw -> draw ()) !overlays;
             R.end_drawing ()
           done
         with
        | exn ->
            R.close_window ();
            raise (Runtime_error ("GUI runtime failure: " ^ Printexc.to_string exn)));
        R.close_window ();
        V_null
      end
  | _ -> assert false

(* Interpreted. A handler is parsed and run against the environment as it stood
   when the window was built, with the widget and the value bound by name. *)
let execute_gui interp block =
  let captured_env = interp.env in
  Gui_tree.build_handler :=
    (fun name source ->
      let body = parse_event_block name source in
      V_native
        (fun arguments ->
          let event_env = child_env captured_env in
          let widget, value =
            match arguments with [ widget; value ] -> (widget, value) | _ -> (V_null, V_null)
          in
          env_define event_env "event_widget" widget;
          env_define event_env "event_value" value;
          with_scope interp event_env (fun () -> execute_block interp body)));
  run_window
    ~publish:(fun name value -> env_define interp.env name value)
      (* A window only opens from the top level of the program being run. One
         inside a function, or at the top of an imported module, builds its
         widgets and hands them over without taking the screen. *)
    ~should_run:(interp.env == interp.globals)
    block

(* Compiled. The handlers arrive already translated, in the order the compiler
   met them, and a property that named the third one carries "2". The widgets a
   window names are published into the interpreter's globals, which is where a
   compiled program's unknown names are looked up -- so 'display' resolves to the
   label the window just made. *)
let run_compiled_window ~interpreter ~handlers ~should_run block =
  Gui_tree.build_handler :=
    (fun name source ->
      match int_of_string_opt source with
      | Some index when index >= 0 && index < Array.length handlers -> handlers.(index)
      | _ ->
          raise (Runtime_error (Printf.sprintf "Compiled handler for '%s' is missing" name)));
  run_window
    ~publish:(fun name value -> env_define interpreter.Interpreter.globals name value)
    ~should_run block

(* --- the draw module -------------------------------------------------------

   What a Canvas is painted with. Every verb comes in two forms, and the plural
   is the one that matters: 'draw.rect' takes one rectangle, 'draw.rects' takes a
   flat list of numbers and reads five at a time. A sand simulator with ten
   thousand grains makes one call per frame rather than ten thousand, so the loop
   over the grains runs here, in OCaml, and not through the language.

   Coordinates start at the canvas's top left. A colour is either the text a
   style would use, "#1e1e1e", or the same thing as a number, 0 to 16777215 --
   the number is there because a flat list of numbers has nowhere to put a
   string, and because a simulation picking colours per cell should not be
   building strings to do it. *)

let canvas_now name =
  match !current_canvas with
  | Some node -> node
  | None ->
      raise
        (Runtime_error
           (Printf.sprintf "draw.%s can only be called from a Canvas's onDraw handler" name))

let draw_color name value =
  match value with
  | V_string text -> (
      match parse_hex_color text with
      | Some (r, g, b, a) -> R.Color.create r g b a
      | None -> raise (Runtime_error (Printf.sprintf "draw.%s does not understand the colour '%s'" name text)))
  | V_int packed ->
      R.Color.create ((packed lsr 16) land 0xFF) ((packed lsr 8) land 0xFF) (packed land 0xFF) 255
  | other ->
      raise
        (Runtime_error
           (Printf.sprintf "draw.%s expects a colour as text or a number, got %s" name
              (type_of_value other)))

let draw_number name value =
  match value with
  | V_int n -> float_of_int n
  | V_float f -> f
  | other ->
      raise
        (Runtime_error
           (Printf.sprintf "draw.%s expects numbers, got %s" name (type_of_value other)))

(* Reads a flat list [stride] entries at a time, handing each group to [shape]
   with the canvas offset already applied. A length that is not a multiple of the
   stride is a mistake worth naming rather than a group quietly dropped. *)
let batched name stride shape values =
  let node = canvas_now name in
  let items = match values with
    | V_list items -> items
    | other ->
        raise
          (Runtime_error
             (Printf.sprintf "draw.%s expects a list of numbers, got %s" name
                (type_of_value other)))
  in
  let count = list_length items in
  if count mod stride <> 0 then
    raise
      (Runtime_error
         (Printf.sprintf "draw.%s reads %d numbers per shape, and was given %d" name stride count));
  let index = ref 0 in
  while !index < count do
    let at offset = list_get items (!index + offset) in
    shape node at;
    index := !index + stride
  done;
  V_null

let register_draw_module () =
  let number name value = draw_number name value in
  let native name arity fn =
    ( name,
      V_native
        (fun arguments ->
          if List.length arguments <> arity then
            raise
              (Runtime_error
                 (Printf.sprintf "draw.%s expects %d argument(s)" name arity));
          fn arguments) )
  in
  Runtime.register_native_module "draw"
    [
      (* The surface's own size, so a program can lay itself out inside it. *)
      ("width", V_native (fun _ -> V_float (canvas_now "width").width));
      ("height", V_native (fun _ -> V_float (canvas_now "height").height));
      native "clear" 1 (fun arguments ->
          let node = canvas_now "clear" in
          let color = draw_color "clear" (List.nth arguments 0) in
          R.draw_rectangle_rec (rect_of node) color;
          V_null);
      native "rect" 5 (fun arguments ->
          let node = canvas_now "rect" in
          let n index = number "rect" (List.nth arguments index) in
          R.draw_rectangle_rec
            (R.Rectangle.create (node.x +. n 0) (node.y +. n 1) (n 2) (n 3))
            (draw_color "rect" (List.nth arguments 4));
          V_null);
      ( "rects",
        V_native (fun arguments ->
            batched "rects" 5 (fun node at ->
                R.draw_rectangle_rec
                  (R.Rectangle.create
                     (node.x +. draw_number "rects" (at 0))
                     (node.y +. draw_number "rects" (at 1))
                     (draw_number "rects" (at 2))
                     (draw_number "rects" (at 3)))
                  (draw_color "rects" (at 4)))
              (List.nth arguments 0)) );
      native "circle" 4 (fun arguments ->
          let node = canvas_now "circle" in
          let n index = number "circle" (List.nth arguments index) in
          R.draw_circle_v
            (R.Vector2.create (node.x +. n 0) (node.y +. n 1))
            (n 2)
            (draw_color "circle" (List.nth arguments 3));
          V_null);
      ( "circles",
        V_native (fun arguments ->
            batched "circles" 4 (fun node at ->
                R.draw_circle_v
                  (R.Vector2.create
                     (node.x +. draw_number "circles" (at 0))
                     (node.y +. draw_number "circles" (at 1)))
                  (draw_number "circles" (at 2))
                  (draw_color "circles" (at 3)))
              (List.nth arguments 0)) );
      native "line" 6 (fun arguments ->
          let node = canvas_now "line" in
          let n index = number "line" (List.nth arguments index) in
          R.draw_line_ex
            (R.Vector2.create (node.x +. n 0) (node.y +. n 1))
            (R.Vector2.create (node.x +. n 2) (node.y +. n 3))
            (n 4)
            (draw_color "line" (List.nth arguments 5));
          V_null);
      ( "lines",
        V_native (fun arguments ->
            batched "lines" 6 (fun node at ->
                R.draw_line_ex
                  (R.Vector2.create
                     (node.x +. draw_number "lines" (at 0))
                     (node.y +. draw_number "lines" (at 1)))
                  (R.Vector2.create
                     (node.x +. draw_number "lines" (at 2))
                     (node.y +. draw_number "lines" (at 3)))
                  (draw_number "lines" (at 4))
                  (draw_color "lines" (at 5)))
              (List.nth arguments 0)) );
      (* A pixel is a one-by-one rectangle rather than draw_pixel, so that a
         canvas scaled by its styles still shows something. *)
      ( "pixels",
        V_native (fun arguments ->
            batched "pixels" 3 (fun node at ->
                R.draw_rectangle_rec
                  (R.Rectangle.create
                     (node.x +. draw_number "pixels" (at 0))
                     (node.y +. draw_number "pixels" (at 1))
                     1. 1.)
                  (draw_color "pixels" (at 2)))
              (List.nth arguments 0)) );
      native "text" 5 (fun arguments ->
          let node = canvas_now "text" in
          let n index = number "text" (List.nth arguments index) in
          let content = value_to_string (List.nth arguments 2) in
          let size = n 3 in
          let font =
            match !active_fonts with
            | Some fonts -> fonts.regular
            | None -> raise (Runtime_error "draw.text has no typeface to write with")
          in
          R.draw_text_ex font content
            (R.Vector2.create (node.x +. n 0) (node.y +. n 1))
            size 0.
            (draw_color "text" (List.nth arguments 4));
          V_null);
    ]

(* --- the scene module ------------------------------------------------------

   Shapes, lights and the camera. A shape's last two arguments are its colour and
   how solid it is: 1 is opaque, anything less is glass, which is set aside and
   drawn after the solids from the furthest away forward. *)

let scene_now name =
  if not scene.inside then
    raise
      (Runtime_error
         (Printf.sprintf "scene.%s can only be called from a Scene's onDraw handler" name));
  match scene.camera with
  | Some camera -> camera
  | None -> raise (Runtime_error "the scene has no camera")

let scene_number name value = draw_number name value

let scene_colour name value alpha =
  let base = draw_color name value in
  R.Color.create (R.Color.r base) (R.Color.g base) (R.Color.b base)
    (int_of_float (Float.max 0. (Float.min 1. alpha) *. 255.))

(* How far a shape is from the eye, for ordering the glass. *)
let depth_from camera x y z =
  let eye = R.Camera3D.position camera in
  let dx = R.Vector3.x eye -. x and dy = R.Vector3.y eye -. y and dz = R.Vector3.z eye -. z in
  (dx *. dx) +. (dy *. dy) +. (dz *. dz)

(* Solid now, glass later. Either way the shape is described once, here. *)
let place camera ~x ~y ~z ~alpha draw =
  if alpha >= 0.999 then draw ()
  else scene.deferred <- (depth_from camera x y z, draw) :: scene.deferred

let register_scene_module () =
  let native name arity fn =
    ( name,
      V_native
        (fun arguments ->
          if List.length arguments <> arity then
            raise (Runtime_error (Printf.sprintf "scene.%s expects %d argument(s)" name arity));
          fn arguments) )
  in
  let glass_uniform alpha =
    match scene.shader with
    | Some shader -> uniform_float shader scene.loc_glassiness (if alpha >= 0.999 then 0.0 else 1.0)
    | None -> ()
  in
  let surface alpha shininess =
    (match scene.shader with
    | Some shader -> uniform_float shader scene.loc_shininess shininess
    | None -> ());
    glass_uniform alpha
  in
  Runtime.register_native_module "scene"
    [
      (* Where the eye is and what it looks at. *)
      native "camera" 6 (fun arguments ->
          let camera = scene_now "camera" in
          let n index = scene_number "camera" (List.nth arguments index) in
          R.Camera3D.set_position camera (R.Vector3.create (n 0) (n 1) (n 2));
          R.Camera3D.set_target camera (R.Vector3.create (n 3) (n 4) (n 5));
          V_null);
      (* Up to four. Beyond that the extra ones are ignored rather than refused,
         since a scene gaining a fifth light should dim, not stop. *)
      native "light" 5 (fun arguments ->
          ignore (scene_now "light");
          let n index = scene_number "light" (List.nth arguments index) in
          let colour = draw_color "light" (List.nth arguments 3) in
          let strength = n 4 in
          scene.lights <-
            ( n 0, n 1, n 2,
              float_of_int (R.Color.r colour) /. 255. *. strength,
              float_of_int (R.Color.g colour) /. 255. *. strength,
              float_of_int (R.Color.b colour) /. 255. *. strength )
            :: scene.lights;
          V_null);
      (* How bright the unlit side of everything is. *)
      native "ambient" 2 (fun arguments ->
          ignore (scene_now "ambient");
          let colour = draw_color "ambient" (List.nth arguments 0) in
          let strength = scene_number "ambient" (List.nth arguments 1) in
          (match scene.shader with
          | Some shader ->
              uniform_vec3 shader scene.loc_ambient
                (float_of_int (R.Color.r colour) /. 255. *. strength)
                (float_of_int (R.Color.g colour) /. 255. *. strength)
                (float_of_int (R.Color.b colour) /. 255. *. strength)
          | None -> ());
          V_null);
      native "cube" 9 (fun arguments ->
          let camera = scene_now "cube" in
          let n index = scene_number "cube" (List.nth arguments index) in
          let x = n 0 and y = n 1 and z = n 2 in
          let width = n 3 and height = n 4 and depth = n 5 in
          let alpha = n 7 and shine = n 8 in
          let colour = scene_colour "cube" (List.nth arguments 6) alpha in
          place camera ~x ~y ~z ~alpha (fun () ->
              surface alpha shine;
              R.draw_cube (R.Vector3.create x y z) width height depth colour);
          V_null);
      native "sphere" 6 (fun arguments ->
          let camera = scene_now "sphere" in
          let n index = scene_number "sphere" (List.nth arguments index) in
          let x = n 0 and y = n 1 and z = n 2 in
          let radius = n 3 in
          let alpha = n 5 in
          let colour = scene_colour "sphere" (List.nth arguments 4) alpha in
          place camera ~x ~y ~z ~alpha (fun () ->
              surface alpha 48.;
              R.draw_sphere_ex (R.Vector3.create x y z) radius 24 24 colour);
          V_null);
      native "cylinder" 7 (fun arguments ->
          let camera = scene_now "cylinder" in
          let n index = scene_number "cylinder" (List.nth arguments index) in
          let x = n 0 and y = n 1 and z = n 2 in
          let radius = n 3 and height = n 4 in
          let alpha = n 6 in
          let colour = scene_colour "cylinder" (List.nth arguments 5) alpha in
          place camera ~x ~y ~z ~alpha (fun () ->
              surface alpha 32.;
              R.draw_cylinder (R.Vector3.create x y z) radius radius height 24 colour);
          V_null);
      native "floor" 5 (fun arguments ->
          let camera = scene_now "floor" in
          let n index = scene_number "floor" (List.nth arguments index) in
          let y = n 0 in
          let width = n 1 and depth = n 2 in
          let alpha = n 4 in
          let colour = scene_colour "floor" (List.nth arguments 3) alpha in
          place camera ~x:0. ~y ~z:0. ~alpha (fun () ->
              surface alpha 8.;
              R.draw_plane (R.Vector3.create 0. y 0.) (R.Vector2.create width depth) colour);
          V_null);
    ]

let register_gui_builtins interp =
  let define name fn = env_define interp.globals name (V_native fn) in
  define "set_text"
    (function
      | [ target; value ] ->
          let node = node_of_value "set_text" target in
          node.text <- value_to_string value;
          node.caret <- String.length node.text;
          target
      | _ -> raise (Runtime_error "set_text expects (widget, value)"));
  define "get_text"
    (function
      | [ target ] -> V_string (node_of_value "get_text" target).text
      | _ -> raise (Runtime_error "get_text expects (widget)"));
  define "set_visible"
    (function
      | [ target; V_bool visible ] ->
          (node_of_value "set_visible" target).visible <- visible;
          target
      | _ -> raise (Runtime_error "set_visible expects (control, bool)"));
  (* A select reports the entry itself rather than its index: the script wrote
     the strings, so the string is what it can act on. *)
  define "get_value"
    (function
      | [ target ] -> (
          let node = node_of_value "get_value" target in
          match node.kind with
          | RlSelect when Array.length node.options > 0 ->
              V_string node.options.(min node.selected (Array.length node.options - 1))
          | RlSlider -> V_int (int_of_float node.slider_value)
          | RlCheckbox -> V_bool node.checked
          | _ -> V_string node.text)
      | _ -> raise (Runtime_error "get_value expects (widget)"));
  (* get_text/set_text mean nothing for a checkbox, so it gets its own pair
     rather than overloading them -- as in the Bogue backend. *)
  define "get_state"
    (function
      | [ target ] -> V_bool (node_of_value "get_state" target).checked
      | _ -> raise (Runtime_error "get_state expects (widget)"));
  define "set_state"
    (function
      | [ target; V_bool state ] ->
          (node_of_value "set_state" target).checked <- state;
          target
      | _ -> raise (Runtime_error "set_state expects (widget, bool)"));
  define "set_enabled"
    (function
      | [ target; V_bool enabled ] ->
          (node_of_value "set_enabled" target).enabled <- enabled;
          target
      | _ -> raise (Runtime_error "set_enabled expects (control, bool)"));
  define "set_width"
    (function
      | [ target; V_int width ] when width > 0 ->
          (node_of_value "set_width" target).forced_width <- Some (float_of_int width);
          target
      | [ _; V_int _ ] -> raise (Runtime_error "set_width expects a positive width")
      | _ -> raise (Runtime_error "set_width expects (control, width)"));
  define "set_height"
    (function
      | [ target; V_int height ] when height > 0 ->
          (node_of_value "set_height" target).forced_height <- Some (float_of_int height);
          target
      | [ _; V_int _ ] -> raise (Runtime_error "set_height expects a positive height")
      | _ -> raise (Runtime_error "set_height expects (control, height)"));
  (* --- file dialogs -------------------------------------------------------

     raylib has no file picker at all, and neither had Bogue in the shape a
     script wants -- one that blocks and hands back a path. So these shell out
     to whichever native picker the desktop provides, exactly as in 0.6.
     Returns none on cancel, so a caller can tell "cancelled" from "chose". *)
  let run_picker command =
    let channel = Unix.open_process_in command in
    let line = try Some (input_line channel) with End_of_file -> None in
    match (Unix.close_process_in channel, line) with
    | Unix.WEXITED 0, Some path when String.trim path <> "" -> Some (String.trim path)
    | _ -> None
  in
  let has_tool name = Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" name) = 0 in
  let pick ~save ~title =
    let quoted = Filename.quote title in
    if has_tool "zenity" then
      run_picker
        (Printf.sprintf "zenity --file-selection %s --title=%s 2>/dev/null"
           (if save then "--save --confirm-overwrite" else "")
           quoted)
    else if has_tool "kdialog" then
      run_picker
        (Printf.sprintf "kdialog --title %s %s . 2>/dev/null" quoted
           (if save then "--getsavefilename" else "--getopenfilename"))
    else
      raise
        (Runtime_error "No file dialog available. Install zenity (GTK) or kdialog (KDE).")
  in
  let dialog_builtin name ~save =
    define name (function
      | [] -> ( match pick ~save ~title:name with Some path -> V_string path | None -> V_null)
      | [ V_string title ] -> (
          match pick ~save ~title with Some path -> V_string path | None -> V_null)
      | _ -> raise (Runtime_error (name ^ " expects () or (title)")))
  in
  dialog_builtin "choose_file" ~save:false;
  dialog_builtin "choose_save_file" ~save:true;
  define "set_value"
    (function
      | [ target; value ] ->
          let node = node_of_value "set_value" target in
          (match (node.kind, value) with
          | RlSlider, V_int number ->
              node.slider_value <- Float.max 0. (Float.min node.slider_max (float_of_int number))
          | RlCheckbox, V_bool state -> node.checked <- state
          | _ -> node.text <- value_to_string value);
          target
      | _ -> raise (Runtime_error "set_value expects (widget, value)"));
  ()

let register () =
  gui_hook := execute_gui;
  register_draw_module ();
  register_scene_module ();
  extra_builtins := !extra_builtins @ [ register_gui_builtins ]
