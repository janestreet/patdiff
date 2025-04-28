open! Core
open! Import
include Html_output_intf

module Make (Mtime : Mtime) = struct
  let string_of_color ?(background = false) : Format.Color.t -> string = function
    | Standard Black -> "#000000"
    | Standard Red -> "#880000"
    | Standard Green -> "#008800"
    | Standard Yellow -> "#888800"
    | Standard Blue -> "#000088"
    | Standard Magenta -> "#880088"
    | Standard Cyan -> "#008888"
    | Standard White -> "#ffffff"
    | Default -> if background then "#000000" else "#ffffff"
    | Bright Black -> "#c0c0c0"
    | Bright Red -> "#FF0000"
    | Bright Green -> "#00FF00"
    | Bright Yellow -> "#FFFF00"
    | Bright Blue -> "#0000FF"
    | Bright Magenta -> "#FF00FF"
    | Bright Cyan -> "#00FFFF"
    | Bright White -> "#FFFFFF"
    | Rgb6 rgb ->
      let r, g, b = Ansi_text.Color.Rgb6.to_rgb rgb in
      let scaled x = x * 255 / 5 in
      sprintf "rgb(%d,%d,%d)" (scaled r) (scaled g) (scaled b)
    | Gray24 gray ->
      let level = Ansi_text.Color.Gray24.to_level gray in
      let scaled = level * 255 / 23 in
      sprintf "rgb(%d,%d,%d)" scaled scaled scaled
    | Rgb256 rgb ->
      let r, g, b = Ansi_text.Color.Rgb256.to_rgb rgb in
      sprintf "rgb(%d,%d,%d)" r g b
  ;;

  module Style = struct
    let apply text ~styles =
      let start_tags, end_tags =
        List.fold styles ~init:([], []) ~f:(fun (s, e) style ->
          match (style : Format.Style.t) with
          | Bold -> "<span style=\"font-weight:bold\">" :: s, "</span>" :: e
          | Reset -> s, e
          | Fg c ->
            sprintf "<span style=\"color:%s\">" (string_of_color c) :: s, "</span>" :: e
          | Bg c ->
            ( sprintf "<span style=\"background-color:%s\">" (string_of_color c) :: s
            , "</span>" :: e )
          | Underline | Italic -> "<u>" :: s, "</u>" :: e
          | Blink -> "<span style=\"text-decoration:blink\">" :: s, "</span>" :: e
          | Invert -> s, e
          | Hide -> "<!-- " :: s, " -->" :: e
          | Faint ->
            (* "<span style=\"font-weight:lighter\">"::s, "</span>"::e *)
            ( sprintf "<span style=\"color:%s\">" (string_of_color (Bright Black)) :: s
            , "</span>" :: e )
          | _ -> s, e)
      in
      let lst = start_tags @ [ text ] @ end_tags in
      String.concat ~sep:"" lst
    ;;
  end

  (* assuming we only insert text in contents and not in attributes, only escaping these
     three characters should be enough. We may want to print differently non printable
     ascii characters too? *)
  let html_escape_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c -> String.of_char c
  ;;

  let html_escape s = String.concat_map s ~f:html_escape_char

  module Rule = struct
    let apply text ~(rule : Format.Rule.t) ~refined =
      let apply styles text = Style.apply text ~styles in
      sprintf
        "%s%s%s"
        (apply rule.pre.styles rule.pre.text)
        (if refined then apply [ Reset ] text else apply rule.styles (html_escape text))
        (apply rule.suf.styles rule.suf.text)
    ;;
  end

  let print_header ~(rules : Format.Rules.t) ~file_names:(prev_file, next_file) ~print =
    let print_line file rule =
      let get_time file =
        match Mtime.mtime file with
        | Ok time -> Time_float.to_string_utc time
        | Error _ -> ""
      in
      let time = get_time file in
      print (Rule.apply (sprintf !"%{File_name#hum} %s" file time) ~rule ~refined:false)
    in
    print_line prev_file rules.header_prev;
    print_line next_file rules.header_next
  ;;

  let print
    ~print_global_header
    ~file_names:((prev_file, _) as file_names)
    ~(rules : Format.Rules.t)
    ~print
    ~location_style
    hunks
    =
    print "<pre style=\"font-family:consolas,monospace\">";
    if print_global_header then print_header ~rules ~file_names ~print;
    let f hunk =
      Format.Location_style.sprint
        location_style
        hunk
        ~prev_filename:(File_name.display_name prev_file)
        ~rule:(Rule.apply ~rule:rules.hunk ~refined:false)
      |> print;
      let handle_range : string Patience_diff.Range.t -> unit = function
        (* Just print the new array elements *)
        | Same r ->
          let mr = Array.map r ~f:snd in
          Array.iter mr ~f:print
        | Prev (r, _) | Next (r, _) | Unified (r, _) -> Array.iter r ~f:print
        | Replace (ar1, ar2, _) ->
          Array.iter ar1 ~f:print;
          Array.iter ar2 ~f:print
      in
      List.iter hunk.ranges ~f:handle_range
    in
    List.iter hunks ~f;
    print "</pre>"
  ;;
end

module Without_mtime = Make (struct
    let mtime _ = Or_error.error_string "Mtime implementation not available"
  end)

module Private = struct
  module Make = Make
end
