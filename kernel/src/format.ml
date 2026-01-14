open! Core
open! Import
module Color = Ansi_text.Color
module Style = Ansi_text.Attr

module Rule = struct
  module Affix = struct
    type t =
      { text : string
      ; styles : Style.t list
      }
    [@@deriving compare ~localize, sexp_of]

    let create ?(styles = []) text = { text; styles }
    let blank = create ""
    let strip_styles t = { t with styles = [] }
  end

  type t =
    { pre : Affix.t
    ; suf : Affix.t
    ; styles : Style.t list
    }
  [@@deriving compare ~localize, fields ~iterators:map, sexp_of]

  let create ?(pre = Affix.blank) ?(suf = Affix.blank) styles = { pre; suf; styles }
  let blank = create []
  let unstyled_prefix text = { blank with pre = Affix.create text }

  let strip_styles t =
    let f f field = f (Field.get field t) in
    Fields.map
      ~pre:(f Affix.strip_styles)
      ~suf:(f Affix.strip_styles)
      ~styles:(f (const []))
  ;;

  let strip_prefix t = { t with pre = Affix.blank }

  let use_prefix_text_from t ~this_prefix =
    { t with pre = { t.pre with text = this_prefix.pre.text } }
  ;;
end

module Rules = struct
  type t =
    { line_same : Rule.t
    ; line_prev : Rule.t
    ; line_next : Rule.t
    ; line_unified : Rule.t
    ; word_same_prev : Rule.t
    ; word_same_next : Rule.t
    ; word_same_unified : Rule.t
    ; word_same_unified_in_move : Rule.t
    ; word_prev : Rule.t
    ; word_next : Rule.t
    ; hunk : Rule.t
    ; header_prev : Rule.t
    ; header_next : Rule.t
    ; moved_from_prev : Rule.t
    ; moved_to_next : Rule.t
    ; removed_in_move : Rule.t
    ; added_in_move : Rule.t
    ; line_unified_in_move : Rule.t
    }
  [@@deriving compare ~localize, fields ~iterators:map, sexp_of]

  module Color_palette = struct
    type t =
      { added : Color.t
      ; removed : Color.t
      ; moved_from_prev : Color.t
      ; moved_to_next : Color.t
      }

    let default =
      let open Ansi_text.Color in
      { added = Standard Sgr8.Green
      ; removed = Standard Sgr8.Red
      ; moved_from_prev = Standard Sgr8.Magenta
      ; moved_to_next = Standard Sgr8.Cyan
      }
    ;;
  end

  let inner_line_change text color =
    let style = [ Ansi_text.Attr.Fg color ] in
    let pre = Rule.Affix.create ~styles:[ Bold; Fg color ] text in
    Rule.create ~pre style
  ;;

  let line_unified ~is_move =
    let pre =
      Rule.Affix.create
        ~styles:[ Bold; Ansi_text.(Attr.Fg (Color.Standard Color.Sgr8.Yellow)) ]
        (if is_move then ">|" else "!|")
    in
    Rule.create ~pre []
  ;;

  let word_change color = Rule.create [ Fg color ]

  let default_with_color_palette (palette : Color_palette.t) =
    let open Rule in
    { line_same = unstyled_prefix "  "
    ; line_prev = inner_line_change "-|" palette.removed
    ; line_next = inner_line_change "+|" palette.added
    ; line_unified = line_unified ~is_move:false
    ; word_same_prev = blank
    ; word_same_next = blank
    ; word_same_unified = blank
    ; word_same_unified_in_move = blank
    ; word_prev = word_change palette.removed
    ; word_next = word_change palette.added
    ; hunk = blank
    ; header_prev = blank
    ; header_next = blank
    ; moved_from_prev = inner_line_change "<|" palette.moved_from_prev
    ; moved_to_next = inner_line_change ">|" palette.moved_to_next
    ; removed_in_move = inner_line_change ">|" palette.removed
    ; added_in_move = inner_line_change ">|" palette.added
    ; line_unified_in_move = line_unified ~is_move:true
    }
  ;;

  let default = default_with_color_palette Color_palette.default

  let strip_styles t =
    let f field = Rule.strip_styles (Field.get field t) in
    Fields.map
      ~line_same:f
      ~line_prev:f
      ~line_next:f
      ~line_unified:f
      ~word_same_prev:f
      ~word_same_next:f
      ~word_same_unified:f
      ~word_same_unified_in_move:f
      ~word_prev:f
      ~word_next:f
      ~hunk:f
      ~header_prev:f
      ~header_next:f
      ~moved_from_prev:f
      ~moved_to_next:f
      ~removed_in_move:f
      ~added_in_move:f
      ~line_unified_in_move:f
  ;;
end

module Location_style = struct
  type t =
    | Diff
    | Omake
    | None
    | Separator
  [@@deriving bin_io, compare ~localize, quickcheck, enumerate, equal ~localize, sexp]

  let to_string = function
    | Diff -> "diff"
    | Omake -> "omake"
    | None -> "none"
    | Separator -> "separator"
  ;;

  let of_string = function
    | "diff" -> Diff
    | "omake" -> Omake
    | "none" -> None
    | "separator" -> Separator
    | other -> failwiths "invalid location style" other [%sexp_of: string]
  ;;

  let omake_style_error_message_start ~file ~line =
    sprintf "File \"%s\", line %d, characters 0-1:" file line
  ;;

  let sprint t (hunk : string Patience_diff.Hunk.t) ~prev_filename ~rule =
    match t with
    | Diff ->
      rule
        (sprintf
           "-%i,%i +%i,%i"
           hunk.prev_start
           hunk.prev_size
           hunk.next_start
           hunk.next_size)
    (* omake locations must be parseable, so we can't let the user config insert arbitrary
       prefixes and suffixes and ANSI color rubbish. *)
    | Omake ->
      (* Print line number of first difference, skipping past context lines. *)
      let prev_start =
        with_return (fun r ->
          List.fold hunk.ranges ~init:hunk.prev_start ~f:(fun init -> function
            | Same s -> init + Array.length s
            | Prev _ | Next _ | Replace _ | Unified _ -> r.return init))
      in
      omake_style_error_message_start ~file:prev_filename ~line:prev_start
    | None -> rule ""
    | Separator -> rule "=== DIFF HUNK ==="
  ;;
end
