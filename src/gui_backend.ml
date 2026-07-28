(* Bogue-backed GUI backend for Suchu.

   Split out of interpreter.ml so that the evaluator itself carries no GUI
   dependency. That lets suchu_eval be compiled to JavaScript with js_of_ocaml
   for the web playground, while this module keeps the native desktop path
   unchanged.

   The dependency runs one way only: this module calls into Interpreter, never
   the reverse. Interpreter reaches the GUI through the [gui_hook] reference
   that [register] installs below. *)

open Ast
open Runtime
open Interpreter

module B = Bogue
module W = B.Widget
module L = B.Layout
module Trigger = B.Trigger
module Draw = B.Draw
module Style = B.Style
module Label = B.Label
module Ttf = Tsdl_ttf.Ttf

type gui_event =
  | GuiOnClick of block
  | GuiOnInput of block
  | GuiOnHover of block

type gui_styles = (string * gui_literal) list

type layout_orientation =
  | OrientationColumn
  | OrientationRow

type gui_window = {
  title : string;
  id : string option;
  width : int option;
  height : int option;
  orientation : layout_orientation;
  styles : gui_styles;
  children : gui_node list;
}

and gui_container = {
  id : string option;
  styles : gui_styles;
  children : gui_node list;
}

and gui_label = {
  id : string option;
  text : string;
  styles : gui_styles;
}

and gui_button = {
  id : string option;
  label : string;
  styles : gui_styles;
  events : gui_event list;
}

and gui_input = {
  id : string option;
  placeholder : string option;
  styles : gui_styles;
  events : gui_event list;
}

and gui_node =
  | GuiWindow of gui_window
  | GuiColumn of gui_container
  | GuiRow of gui_container
  | GuiLabel of gui_label
  | GuiButton of gui_button
  | GuiInput of gui_input

type gui_control = {
  widget : W.t;
  layout : L.t;
}
let control_widget name = function
  | V_object object_value when String.equal object_value.kind "control" ->
      let control : gui_control = Obj.obj object_value.payload in
      control.widget
  | value -> expect_object name "widget" value

let control_layout name = function
  | V_object object_value when String.equal object_value.kind "control" ->
      let control : gui_control = Obj.obj object_value.payload in
      control.layout
  | value -> expect_object name "layout" value

let lowercase s = String.lowercase_ascii s
let separate_props props =
  List.fold_left
    (fun (literals, events) (name, value) ->
      match value with
      | GuiPropLiteral lit -> ((name, lit) :: literals, events)
      | GuiPropEvent source -> (literals, (name, source) :: events))
    ([], [])
    props
  |> fun (literals, events) -> (List.rev literals, List.rev events)

let find_literal name literals =
  let target = lowercase name in
  List.find_map
    (fun (key, value) -> if lowercase key = target then Some value else None)
    literals

let rec find_first_literal names literals =
  match names with
  | [] -> None
  | key :: rest -> (
      match find_literal key literals with
      | Some literal -> Some literal
      | None -> find_first_literal rest literals)

let filter_out excluded literals =
  List.filter (fun (name, _) -> not (List.exists (fun key -> key = name) excluded)) literals

let round_to_int f =
  if f >= 0. then int_of_float (Float.floor (f +. 0.5))
  else int_of_float (Float.ceil (f -. 0.5))

let text_literal = function
  | GuiString s -> Some s
  | GuiIdent s -> Some s
  | _ -> None

let int_from_literal = function
  | GuiLength px -> Some px
  | GuiNumber f -> Some (round_to_int f)
  | GuiString s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let parse_hex_color value =
  let hex =
    let raw =
      if String.length value > 0 && value.[0] = '#' then String.sub value 1 (String.length value - 1)
      else value
    in
    match String.length raw with
    | 3 ->
        let expand c = String.make 2 c in
        expand raw.[0] ^ expand raw.[1] ^ expand raw.[2]
    | 6 -> raw
    | _ -> raw
  in
  if String.length hex <> 6 then None
  else
    try
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Some (r, g, b, 255)
    with _ -> None

let color_from_literal = function
  | GuiColor hex -> parse_hex_color hex
  | GuiString s when String.length s > 0 && s.[0] = '#' -> parse_hex_color s
  | _ -> None

let literal_to_string = function
  | GuiString s -> s
  | GuiBool true -> "true"
  | GuiBool false -> "false"
  | GuiNumber f ->
      let int_candidate = int_of_float f in
      if abs_float (f -. float_of_int int_candidate) < 1e-9 then string_of_int int_candidate
      else string_of_float f
  | GuiLength px -> Printf.sprintf "%dpx" px
  | GuiColor hex -> hex
  | GuiIdent name -> name

let ensure_no_children element_name children =
  match children with
  | [] -> ()
  | _ ->
      raise (Runtime_error (Printf.sprintf "'%s' does not accept child elements" element_name))

let parse_event_block name source =
  try Parser.parse source with
  | Parser.Parse_error message ->
      raise (Runtime_error (Printf.sprintf "Invalid handler for '%s': %s" name message))

let collect_events entries =
  let rec loop acc = function
    | [] -> List.rev acc
    | (name, source) :: rest ->
        let lower = lowercase name in
        let block = parse_event_block lower source in
        let event =
          match lower with
          | "onclick" -> GuiOnClick block
          | "oninput" -> GuiOnInput block
          | "onhover" -> GuiOnHover block
          | _ -> raise (Runtime_error (Printf.sprintf "Unknown GUI event '%s'" name))
        in
        loop (event :: acc) rest
  in
  loop [] entries

let orientation_of_string value =
  match lowercase value with
  | "row"
  | "horizontal" -> OrientationRow
  | _ -> OrientationColumn

let orientation_from_literals literals =
  match Option.bind (find_literal "layout" literals) text_literal with
  | Some value -> orientation_of_string value
  | None -> OrientationColumn

let id_from_args_and_props args literals =
  match Option.bind (find_literal "id" literals) text_literal with
  | Some id -> Some id
  | None -> (
      match args with
      | first :: _ -> text_literal first
      | [] -> None)

let rec convert_gui_block = function
  | GuiBlock (raw_name, args, props, children_blocks) ->
      let name = lowercase raw_name in
      let literal_props, event_entries = separate_props props in
      let children = List.map convert_gui_block children_blocks in
      let default_label_from_args () =
        match args with
        | first :: _ -> Option.value (text_literal first) ~default:(literal_to_string first)
        | [] -> raw_name
      in
      match name with
      | "window" ->
          if event_entries <> [] then
            raise (Runtime_error "window elements do not accept event handlers");
          let title =
            match args with
            | first :: _ -> Option.value (text_literal first) ~default:(literal_to_string first)
            | [] ->
                (match Option.bind (find_literal "title" literal_props) text_literal with
                | Some t -> t
                | None -> raw_name)
          in
          let width =
            let from_args =
              match args with
              | _ :: second :: _ -> int_from_literal second
              | _ -> None
            in
            match from_args with
            | Some value -> Some value
            | None -> Option.bind (find_literal "width" literal_props) int_from_literal
          in
          let height =
            let from_args =
              match args with
              | _ :: _ :: third :: _ -> int_from_literal third
              | _ -> None
            in
            match from_args with
            | Some value -> Some value
            | None -> Option.bind (find_literal "height" literal_props) int_from_literal
          in
          let id_opt = Option.bind (find_literal "id" literal_props) text_literal in
          let orientation = orientation_from_literals literal_props in
          let styles = filter_out [ "id"; "title"; "layout"; "width"; "height" ] literal_props in
          GuiWindow { title; id = id_opt; width; height; orientation; styles; children }
      | "column" ->
          if event_entries <> [] then raise (Runtime_error "column does not support events");
          let id_opt = id_from_args_and_props args literal_props in
          let styles = filter_out [ "id" ] literal_props in
          GuiColumn { id = id_opt; styles; children }
      | "row" ->
          if event_entries <> [] then raise (Runtime_error "row does not support events");
          let id_opt = id_from_args_and_props args literal_props in
          let styles = filter_out [ "id" ] literal_props in
          GuiRow { id = id_opt; styles; children }
      | "label" ->
          ensure_no_children raw_name children_blocks;
          if event_entries <> [] then raise (Runtime_error "label does not support events");
          let text =
            match Option.bind (find_first_literal [ "text"; "label"; "value"; "content" ] literal_props) text_literal with
            | Some value -> value
            | None -> default_label_from_args ()
          in
          let id_opt = Option.bind (find_literal "id" literal_props) text_literal in
          let styles = filter_out [ "id"; "text"; "label"; "value"; "content" ] literal_props in
          GuiLabel { id = id_opt; text; styles }
      | "button" ->
          ensure_no_children raw_name children_blocks;
          let label =
            match Option.bind (find_first_literal [ "text"; "label" ] literal_props) text_literal with
            | Some value -> value
            | None -> default_label_from_args ()
          in
          let id_opt = Option.bind (find_literal "id" literal_props) text_literal in
          let styles = filter_out [ "id"; "text"; "label" ] literal_props in
          let events = collect_events event_entries in
          GuiButton { id = id_opt; label; styles; events }
      | "input" ->
          ensure_no_children raw_name children_blocks;
          let id_opt = Option.bind (find_literal "id" literal_props) text_literal in
          let placeholder =
            match Option.bind (find_first_literal [ "placeholder"; "prompt"; "text" ] literal_props) text_literal with
            | Some value -> Some value
            | None -> (
                match args with
                | first :: _ -> text_literal first
                | [] -> None)
          in
          let styles = filter_out [ "id"; "placeholder"; "prompt"; "text" ] literal_props in
          let events = collect_events event_entries in
          GuiInput { id = id_opt; placeholder; styles; events }
      (* Previously any unrecognised name silently became a button, so a typo
         like `Labell { text: "x"; }` produced a stray button instead of an
         error. Name the mistake and list what is available. *)
      | _ ->
          raise
            (Runtime_error
               (Printf.sprintf
                  "Unknown GUI element '%s'. Available elements: Window, Row, Column, Label, \
                   Button, Input"
                  raw_name))

let convert_gui_root block =
  match convert_gui_block block with
  | GuiWindow _ as window -> window
  | _ -> raise (Runtime_error "GUI statement must start with a window element")

let spacing_from_styles styles =
  Option.bind (find_first_literal [ "spacing"; "gap" ] styles) int_from_literal

let margins_from_styles styles =
  Option.bind (find_first_literal [ "padding"; "margin"; "margins" ] styles) int_from_literal

let align_from_styles styles =
  match Option.bind (find_first_literal [ "align"; "alignment"; "justify-content" ] styles) text_literal with
  | Some value -> (
      match lowercase value with
      | "start"
      | "left"
      | "min" -> Some Draw.Min
      | "end"
      | "right"
      | "max" -> Some Draw.Max
      | "center"
      | "middle" -> Some Draw.Center
      | _ -> None)
  | None -> None

let text_align_from_styles styles =
  match Option.bind (find_first_literal [ "text-align"; "textalign" ] styles) text_literal with
  | Some value -> (
      match lowercase value with
      | "start"
      | "left"
      | "min" -> Some Draw.Min
      | "end"
      | "right"
      | "max" -> Some Draw.Max
      | "center"
      | "middle" -> Some Draw.Center
      | _ -> None)
  | None -> None

let width_from_styles styles =
  Option.bind (find_first_literal [ "width" ] styles) int_from_literal

let height_from_styles styles =
  Option.bind (find_first_literal [ "height" ] styles) int_from_literal

let text_color_from_styles styles =
  Option.bind (find_first_literal [ "color"; "text-color" ] styles) color_from_literal

let font_size_from_styles styles =
  Option.bind (find_first_literal [ "font-size"; "size" ] styles) int_from_literal

let font_style_from_styles styles =
  let weight =
    match Option.bind (find_first_literal [ "font-weight"; "weight" ] styles) text_literal with
    | Some value -> (
        match lowercase value with
        | "bold" -> Some Ttf.Style.bold
        | "normal" -> Some Ttf.Style.normal
        | _ -> None)
    | None -> None
  in
  let slant =
    match Option.bind (find_first_literal [ "font-style" ] styles) text_literal with
    | Some value -> (
        match lowercase value with
        | "italic" -> Some Ttf.Style.italic
        | "underline" -> Some Ttf.Style.underline
        | "strikethrough" -> Some Ttf.Style.strikethrough
        | _ -> None)
    | None -> None
  in
  match (weight, slant) with
  | None, None -> None
  | Some style, None -> Some style
  | None, Some style -> Some style
  | Some a, Some b -> Some Ttf.Style.(a + b)

let layout_background_from_styles styles =
  let color_opt =
    Option.bind (find_first_literal [ "background"; "background-color" ] styles) color_from_literal
  in
  let radius_opt =
    Option.bind (find_first_literal [ "border-radius"; "radius" ] styles) int_from_literal
  in
  match (color_opt, radius_opt) with
  | None, None -> None
  | _ ->
      let style = ref Style.empty in
      (match color_opt with
      | Some color -> style := Style.with_bg (Style.color_bg color) !style
      | None -> ());
      (match radius_opt with
      | Some radius ->
          let line = Style.mk_line ~width:0 ~color:B.RGBA.none () in
          let border = Style.mk_border ~radius:radius line in
          style := Style.with_border border !style
      | None -> ());
      if !style = Style.empty then None else Some (L.style_bg !style)

let apply_layout_styles layout styles =
  (match layout_background_from_styles styles with
  | Some background -> L.set_background layout (Some background)
  | None -> ());
  Option.iter (fun width -> L.set_width layout width) (width_from_styles styles);
  Option.iter (fun height -> L.set_height layout height) (height_from_styles styles)

let button_background_from_styles styles =
  Option.bind (find_first_literal [ "background"; "background-color" ] styles) color_from_literal
  |> Option.map Style.color_bg

let has_any_style names styles =
  List.exists
    (fun (name, _) -> List.exists (fun expected -> String.equal (lowercase name) expected) names)
    styles

let ensure_style names name value styles =
  if has_any_style names styles then styles else (name, value) :: styles

let modern_window_styles styles =
  styles
  |> ensure_style [ "background"; "background-color" ] "background" (GuiColor "#111827")
  |> ensure_style [ "padding"; "margin"; "margins" ] "padding" (GuiNumber 20.)
  |> ensure_style [ "spacing"; "gap" ] "spacing" (GuiNumber 12.)

let modern_input_styles styles =
  styles
  |> ensure_style [ "background"; "background-color" ] "background" (GuiColor "#24283b")
  |> ensure_style [ "border-radius"; "radius" ] "border-radius" (GuiNumber 10.)
  |> ensure_style [ "padding"; "margin"; "margins" ] "padding" (GuiNumber 12.)
  |> ensure_style [ "height" ] "height" (GuiNumber 46.)

let lighten_color amount (red, green, blue, alpha) =
  let lighten channel = min 255 (channel + amount) in
  (lighten red, lighten green, lighten blue, alpha)
let rec execute_gui interp block =
  match convert_gui_root block with
  | GuiWindow window ->
      B.RGBA.set_text_color (226, 232, 240, 255);
      let captured_env = interp.env in
      let child_layouts, child_exports = build_gui_children interp captured_env window.children in
      let window_styles = modern_window_styles window.styles in
      let sep = spacing_from_styles window_styles in
      let margins = margins_from_styles window_styles in
      let align = align_from_styles window_styles in
      let layout_name = Option.value window.id ~default:window.title in
      let root_layout =
        match child_layouts with
        | [] -> L.resident ~name:layout_name (W.label "")
        | _ -> (
            match window.orientation with
            | OrientationColumn -> L.tower ~name:layout_name ?sep ?margins ?align child_layouts
            | OrientationRow -> L.flat ~name:layout_name ?sep ?margins ?align child_layouts)
      in
      apply_layout_styles root_layout window_styles;
      Option.iter (fun width -> L.set_width root_layout width) window.width;
      Option.iter (fun height -> L.set_height root_layout height) window.height;
      let exports =
        match window.id with
        | Some id -> child_exports @ [ (id, make_object "layout" root_layout) ]
        | None -> child_exports
      in
      List.iter (fun (name, value) -> env_define captured_env name value) exports;
      let board = B.Main.of_layout root_layout in
      let should_run = interp.env == interp.globals in
      if should_run then
        try B.Main.run board with
        | exn -> raise (Runtime_error ("GUI runtime failure: " ^ Printexc.to_string exn))
      else ();
      V_null
  | GuiColumn _
  | GuiRow _
  | GuiLabel _
  | GuiButton _
  | GuiInput _ -> assert false
and run_gui_callback interp captured_env ~widget ?(value = V_null) block =
  let event_env = child_env captured_env in
  env_define event_env "event_widget" (make_object "widget" widget);
  env_define event_env "event_value" value;
  with_scope interp event_env (fun () ->
      try ignore (execute_block interp block) with
      | Return_signal _ -> ()
      | Runtime_error msg -> prerr_endline ("GUI runtime error: " ^ msg)
      | exn -> prerr_endline ("GUI callback failed: " ^ Printexc.to_string exn))

and register_hover_events interp captured_env widget block =
  W.mouse_over
    ~enter:(fun w -> run_gui_callback interp captured_env ~widget:w ~value:(V_bool true) block)
    ~leave:(fun w -> run_gui_callback interp captured_env ~widget:w ~value:(V_bool false) block)
    widget

and register_button_events interp captured_env widget events =
  List.iter
    (function
      | GuiOnClick block -> W.on_click ~click:(fun w -> run_gui_callback interp captured_env ~widget:w block) widget
      | GuiOnInput _ -> raise (Runtime_error "button does not support onInput")
      | GuiOnHover block -> register_hover_events interp captured_env widget block)
    events

and register_input_events interp captured_env widget events =
  List.iter
    (function
      | GuiOnClick block ->
          W.on_click ~click:(fun w -> run_gui_callback interp captured_env ~widget:w block) widget
      | GuiOnInput block ->
          let connection =
            W.connect widget widget
              (fun w _ _ ->
                let text = W.get_text w in
                run_gui_callback interp captured_env ~widget:w ~value:(V_string text) block)
              Trigger.[text_input; key_down]
          in
          W.add_connection widget connection
      | GuiOnHover block -> register_hover_events interp captured_env widget block)
    events

and build_gui_children interp captured_env nodes =
  List.fold_left
    (fun (layouts_acc, exports_acc) node ->
      let layout, node_exports = build_gui_node interp captured_env node in
      (layouts_acc @ [ layout ], exports_acc @ node_exports))
    ([], [])
    nodes

and build_gui_node interp captured_env = function
  | GuiColumn container ->
      let child_layouts, child_exports = build_gui_children interp captured_env container.children in
      let sep = spacing_from_styles container.styles in
      let margins = margins_from_styles container.styles in
      let align = align_from_styles container.styles in
      let layout = L.tower ?name:container.id ?sep ?margins ?align child_layouts in
      apply_layout_styles layout container.styles;
      let exports =
        match container.id with
        | Some id -> child_exports @ [ (id, make_object "layout" layout) ]
        | None -> child_exports
      in
      (layout, exports)
  | GuiRow container ->
      let child_layouts, child_exports = build_gui_children interp captured_env container.children in
      let sep = spacing_from_styles container.styles in
      let margins = margins_from_styles container.styles in
      let align = align_from_styles container.styles in
      let layout = L.flat ?name:container.id ?sep ?margins ?align child_layouts in
      apply_layout_styles layout container.styles;
      let exports =
        match container.id with
        | Some id -> child_exports @ [ (id, make_object "layout" layout) ]
        | None -> child_exports
      in
      (layout, exports)
  | GuiLabel label ->
      let size_opt = Some (Option.value (font_size_from_styles label.styles) ~default:16) in
      let fg_opt =
        Some (Option.value (text_color_from_styles label.styles) ~default:(226, 232, 240, 255))
      in
      let style_opt = font_style_from_styles label.styles in
      let align_opt = text_align_from_styles label.styles in
      let widget =
        match style_opt with
        | Some style -> W.label ?size:size_opt ?fg:fg_opt ~style ?align:align_opt label.text
        | None -> W.label ?size:size_opt ?fg:fg_opt ?align:align_opt label.text
      in
      let layout = L.resident ?name:label.id widget in
      apply_layout_styles layout label.styles;
      let exports =
        match label.id with
        | Some id -> [ (id, make_object "control" { widget; layout }) ]
        | None -> []
      in
      (layout, exports)
  | GuiButton button ->
      let fg = Option.value (text_color_from_styles button.styles) ~default:(248, 250, 252, 255) in
      let size = Option.value (font_size_from_styles button.styles) ~default:16 in
      let style_opt = font_style_from_styles button.styles in
      let button_label =
        match style_opt with
        | Some style -> Label.create ~size ~fg ~style button.label
        | None -> Label.create ~size ~fg button.label
      in
      let color =
        Option.value
          (Option.bind
             (find_first_literal [ "background"; "background-color" ] button.styles)
             color_from_literal)
          ~default:(99, 102, 241, 255)
      in
      let background = Style.color_bg color in
      let hover_background = Style.color_bg (lighten_color 18 color) in
      let border_radius =
        Option.value
          (Option.bind (find_first_literal [ "border-radius"; "radius" ] button.styles) int_from_literal)
          ~default:10
      in
      let widget =
        W.button ~fg ~label:button_label ~bg_on:background ~bg_off:background
          ~bg_over:(Some hover_background) ~border_radius
          ~border_color:(lighten_color 10 color) button.label
      in
      register_button_events interp captured_env widget button.events;
      let layout = L.resident ?name:button.id widget in
      apply_layout_styles layout button.styles;
      if height_from_styles button.styles = None then L.set_height layout 44;
      let exports =
        match button.id with
        | Some id -> [ (id, make_object "control" { widget; layout }) ]
        | None -> []
      in
      (layout, exports)
  | GuiInput input ->
      let input_styles = modern_input_styles input.styles in
      let size_opt = font_size_from_styles input_styles in
      let widget =
        match (input.placeholder, size_opt) with
        | Some text, Some size -> W.text_input ~prompt:text ~size ()
        | Some text, None -> W.text_input ~prompt:text ()
        | None, Some size -> W.text_input ~size ()
        | None, None -> W.text_input ()
      in
      register_input_events interp captured_env widget input.events;
      let layout = L.resident ?name:input.id widget in
      apply_layout_styles layout input_styles;
      let exports =
        match input.id with
        | Some id -> [ (id, make_object "control" { widget; layout }) ]
        | None -> []
      in
      (layout, exports)
  | GuiWindow _ -> raise (Runtime_error "Nested windows are not supported")

let register_gui_builtins interp =
  let define name fn = env_define interp.globals name (V_native fn) in
  define "set_text"
    (function
      | [ target; text ] ->
          let widget = control_widget "set_text" target in
          W.set_text widget (value_to_string text);
          target
      | _ -> raise (Runtime_error "set_text expects (widget, value)"));
  define "get_text"
    (function
      | [ target ] ->
          let widget = control_widget "get_text" target in
          V_string (W.get_text widget)
      | _ -> raise (Runtime_error "get_text expects (widget)"));
  define "set_visible"
    (function
      | [ target; V_bool visible ] ->
          let layout = control_layout "set_visible" target in
          L.set_show layout visible;
          target
      | _ -> raise (Runtime_error "set_visible expects (control, bool)"));
  define "set_enabled"
    (function
      | [ target; V_bool enabled ] ->
          let layout = control_layout "set_enabled" target in
          L.set_enabled layout enabled;
          target
      | _ -> raise (Runtime_error "set_enabled expects (control, bool)"));
  define "set_width"
    (function
      | [ target; V_int width ] when width > 0 ->
          let layout = control_layout "set_width" target in
          L.set_width layout width;
          target
      | [ _; V_int _ ] -> raise (Runtime_error "set_width expects a positive width")
      | _ -> raise (Runtime_error "set_width expects (control, width)"));
  define "set_height"
    (function
      | [ target; V_int height ] when height > 0 ->
          let layout = control_layout "set_height" target in
          L.set_height layout height;
          target
      | [ _; V_int _ ] -> raise (Runtime_error "set_height expects a positive height")
      | _ -> raise (Runtime_error "set_height expects (control, height)"));
  ()

(* Installed by Cli at startup. *)
let register () =
  gui_hook := execute_gui;
  extra_builtins := !extra_builtins @ [ register_gui_builtins ]
