open Core.Std

module Patience_diff = Patience_diff_lib.Std.Patience_diff

(* Format is the home of all the internal representations of the formatting
   that will be applied to the diff. ie. prefixes, suffixes, & valid styles.
*)
module Format = struct

  module Color = struct

    type t =
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
    | Gray
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white
    | Cmyk of float * float * float * float
    with sexp

  end

  module Style = struct

    type t =
    | Bold | Underline | Emph
    | Blink | Dim | Inverse | Hide
    | Reset
    | Foreground of Color.t | Fg of Color.t
    | Background of Color.t | Bg of Color.t
    with sexp

  end

  (* A rule consists of a styled prefix, a styled suffix, and a style. Rules
     are applied to strings using functions defined in Output_ops.
  *)
  module Rule = struct

    (* An annex is either a prefix or a suffix. *)
    module Annex = struct

      type t = {
        text: string;
        styles: Style.t list
      }

      let create ?(styles = []) text = {
        text = text;
        styles = styles;
      }

      let blank = create ""

    end

    type t = {
      pre: Annex.t;
      suf: Annex.t;
      styles: Style.t list;
      name: string;
    }


    (* Rule creation: Most rules have a style, and maybe a prefix. For
       instance, a line_new rule might have a bold "+" prefix and a green
       style.
    *)
    let create ?(pre = Annex.blank) ?(suf = Annex.blank) styles ~name = {
      pre = pre;
      suf = suf;
      styles = styles;
      name = name;
    }

    let blank = create []

    let __UNUSED_VALUE__reset = create [Style.Reset]

    let unstyled_prefix text ~name =
      let rule = blank ~name in
      { rule with
        pre = Annex.create text;
      }

  end


  (* Rules are configured in the configuration file.
     Default values are provided in Configuration.
  *)
  module Rules = struct

    type t = {
      line_same: Rule.t;
      line_old: Rule.t;
      line_new: Rule.t;
      line_unified: Rule.t;
      word_same_old: Rule.t;
      word_same_new: Rule.t;
      word_same_unified: Rule.t;
      word_old: Rule.t;
      word_new: Rule.t;
      hunk: Rule.t;
      header_old: Rule.t;
      header_new: Rule.t;
    }

  end
end

(* Currently have two types of output: Ansi and Html *)
module Output = struct
  type t = Ansi | Html with sexp
end

(* An output can apply a style to a string and print a list of hunks *)
module type Output = sig

  module Rule : sig

    val apply :
      string ->
      rule: Format.Rule.t ->
      refined: bool ->
      string

  end

  val print
    :  ?file_names:(string * string)
    -> rules: Format.Rules.t
    -> print:(string -> unit)
    -> string Patience_diff.Hunk.t list
    -> unit
end

module Ansi : sig

  include Output

  val iter
    :  f_hunk_break:((int*int) -> (int*int) -> unit)
    -> f_line:(string -> unit)
    -> string Patience_diff.Hunk.t list
    -> unit

end = struct

  module A = Ansi_terminal.ANSITerminal

  module Color = struct

    let of_internal =
      let module C = Format.Color in
      function
      | C.Black -> A.Black | C.Red -> A.Red | C.Green -> A.Green | C.Yellow -> A.Yellow
      | C.Blue -> A.Blue | C.Magenta -> A.Magenta | C.Cyan -> A.Cyan | C.White -> A.White
      | C.Gray -> A.Bright_black
      | C.Bright_black -> A.Bright_black | C.Bright_red -> A.Bright_red
      | C.Bright_green -> A.Bright_green | C.Bright_yellow -> A.Bright_yellow
      | C.Bright_blue -> A.Bright_blue | C.Bright_magenta -> A.Bright_magenta
      | C.Bright_cyan -> A.Bright_cyan | C.Bright_white -> A.Bright_white
      | C.Default -> A.Default
      | C.Cmyk _ -> A.Default
  end

  module Style = struct

    let of_internal =
      let module S = Format.Style in
      function
      | S.Bold -> A.Bold | S.Underline -> A.Underlined | S.Emph -> A.Underlined
      | S.Blink -> A.Blink | S.Inverse -> A.Inverse | S.Hide -> A.Hidden
      | S.Dim -> A.Dim
      | S.Reset -> A.Reset
      | S.Foreground color | S.Fg color -> A.Foreground (Color.of_internal color)
      | S.Background color | S.Bg color -> A.Background (Color.of_internal color)

    let apply text ~styles =
      match List.map styles ~f:(of_internal) with
      | [] -> text
      | styles -> A.apply_string (A.Reset :: styles) text

  end

  module Rule = struct

    let apply text ~rule ~refined =
      let module R = Format.Rule in
      let module A = Format.Rule.Annex in
      let apply styles text = Style.apply text ~styles in
      sprintf "%s%s%s"
        (apply rule.R.pre.A.styles rule.R.pre.A.text)
        (if refined then apply [Format.Style.Reset] text else apply rule.R.styles text)
        (apply rule.R.suf.A.styles rule.R.suf.A.text)

  end

  let print_header ~rules ~old_file ~new_file ~print =
    let print_line file rule =
      print (Rule.apply file ~rule ~refined:false)
    in
    let module Rz = Format.Rules in
    print_line old_file rules.Rz.header_old;
    print_line new_file rules.Rz.header_new
  ;;

  let iter ~f_hunk_break ~f_line hunks =
    let module R = Patience_diff.Range in
    let module H = Patience_diff.Hunk in
    let f_hunk hunk =
      f_hunk_break
        (hunk.H.mine_start, hunk.H.mine_size)
        (hunk.H.other_start, hunk.H.other_size);
      let module R = Patience_diff.Range in
      let handle_range = function
        (* Just print the new array elements *)
        | R.Same r ->
          let mr = Array.map r ~f:snd in
          Array.iter mr ~f:f_line
        | R.Old r | R.New r | R.Unified r ->
          Array.iter r ~f:f_line
        | R.Replace (ar1, ar2) ->
          Array.iter ar1 ~f:f_line;
          Array.iter ar2 ~f:f_line
      in
      List.iter hunk.H.ranges ~f:handle_range
    in
    List.iter hunks ~f:f_hunk
  ;;

  let print ?file_names ~rules ~print hunks =
    let module Rz = Format.Rules in
    let f_hunk_break (mine_start, mine_size) (other_start, other_size) =
      let hunk_info =
        sprintf "-%i,%i +%i,%i"
          mine_start mine_size
          other_start other_size
      in
      print (Rule.apply hunk_info ~rule:rules.Rz.hunk ~refined:false)
    in
    Option.iter file_names ~f:(fun (old_file, new_file) ->
      print_header ~rules ~old_file ~new_file ~print);
    iter
      ~f_hunk_break
      ~f_line:print
      hunks
  ;;
end

module Html : Output = struct

  let string_of_color c =
    let module C = Format.Color in
    match c with
    | C.Black -> "#000000"
    | C.Red -> "#880000"
    | C.Green -> "#008800"
    | C.Yellow -> "#888800"
    | C.Blue -> "#000088"
    | C.Magenta -> "#880088"
    | C.Cyan -> "#008888"
    | C.White | C.Default -> "#ffffff"
    | C.Gray -> "#c0c0c0"
    | C.Bright_black -> "#c0c0c0"
    | C.Bright_red -> "#FF0000"
    | C.Bright_green -> "#00FF00"
    | C.Bright_yellow -> "#FFFF00"
    | C.Bright_blue -> "#0000FF"
    | C.Bright_magenta -> "#FF00FF"
    | C.Bright_cyan -> "#00FFFF"
    | C.Bright_white -> "#FFFFFF"
    | C.Cmyk (a, b, c, d) ->
      sprintf "cmyk(%f,%f,%f,%f)" a b c d
  ;;

  module Style = struct
    let apply text ~styles =
      let module S = Format.Style in
      let start_tags, end_tags =
        List.fold styles ~init:([],[]) ~f:(fun (s, e) style ->
          match style with
          | S.Bold -> "<span style=\"font-weight:bold\">"::s, "</span>"::e
          (* rdouglass: what is a reset?  I can get nice diffs without it, ignorning for now *)
          | S.Reset -> s, e
          | S.Foreground c | S.Fg c ->
            (sprintf "<span style=\"color:%s\">" (string_of_color c))::s, "</span>"::e
          | S.Background c | S.Bg c ->
            (sprintf "<span style=\"background-color:%s\">" (string_of_color c))::s,
            "</span>"::e
          | S.Underline | S.Emph -> "<u>"::s, "</u>"::e
          | S.Blink -> "<span style=\"text-decoration:blink\">"::s, "</span>"::e
          (* rdouglass: ignoring Inverse for now, can't figure out how it's useful *)
          | S.Inverse -> s, e
          | S.Hide -> "<!-- "::s, " -->"::e
          (* rdouglass: would be nice to use font-weight:lighter here, but doesn't seem to
           * do anything for me running firefox 3.5 on centos 5 *)
          | S.Dim ->
            (* rdouglass: doesn't show up differently, going with the gray below instead *)
            (* "<span style=\"font-weight:lighter\">"::s, "</span>"::e *)

            (sprintf "<span style=\"color:%s\">" (string_of_color Format.Color.Gray))::s,
            "</span>"::e
        )
      in
      let lst = start_tags @ [text] @ end_tags in
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

  let html_escape s =
    String.concat_map s ~f:html_escape_char

  module Rule = struct

    let apply text ~rule ~refined =
      let module R = Format.Rule in
      let module A = Format.Rule.Annex in
      let apply styles text = Style.apply text ~styles in
      sprintf "%s%s%s"
        (apply rule.R.pre.A.styles rule.R.pre.A.text)
        (if refined then apply [Format.Style.Reset] text else apply rule.R.styles (html_escape text))
        (apply rule.R.suf.A.styles rule.R.suf.A.text)

  end

  let print_header ~rules ~old_file ~new_file ~print =
    let print_line file rule =
      let get_time s =
        try Time.to_string (Time.of_float (Unix.stat s).Unix.st_mtime)
        with _e -> "" in
      let time = get_time file in
      print (Rule.apply (file ^ " " ^ time) ~rule ~refined:false)
    in
    let module Rz = Format.Rules in
    print_line old_file rules.Rz.header_old;
    print_line new_file rules.Rz.header_new;
  ;;

  let print ?file_names ~rules ~print hunks =
    print "<pre style=\"font-family:consolas,monospace\">";
    Option.iter file_names ~f:(fun (old_file, new_file) ->
      print_header ~rules ~old_file ~new_file ~print);
    let f hunk =
      let module Rz = Format.Rules in
      let module H = Patience_diff.Hunk in
      let hunk_info =
        sprintf "-%i,%i +%i,%i"
          hunk.H.mine_start hunk.H.mine_size
          hunk.H.other_start hunk.H.other_size
      in
      print (Rule.apply hunk_info ~rule:rules.Rz.hunk ~refined:false);
      let module R = Patience_diff.Range in
      let handle_range = function
        (* Just print the new array elements *)
        | R.Same r ->
          let mr = Array.map r ~f:snd in
          Array.iter mr ~f:print
        | R.Old r | R.New r | R.Unified r ->
          Array.iter r ~f:print
        | R.Replace (ar1, ar2) ->
          Array.iter ar1 ~f:print;
          Array.iter ar2 ~f:print
      in
      List.iter hunk.H.ranges ~f:handle_range
    in
    List.iter hunks ~f;
    print "</pre>";
  ;;
end

module Output_ops = struct

  module Rule = struct

    let apply text ~rule ~output ~refined = match output with
      | Output.Ansi -> Ansi.Rule.apply text ~rule ~refined
      | Output.Html -> Html.Rule.apply text ~rule ~refined

  end

  module Rules = struct

    let to_string rules output =
      let module Rz = Format.Rules in
      let module R = Patience_diff.Range in
      let apply text ~rule ~refined = Rule.apply text ~rule ~output ~refined in
      function
      | R.Same ar ->
        let formatted_ar = Array.map ar
          ~f:(fun (x,y) ->
            let app = apply ~rule:rules.Rz.line_same ~refined:false in
            (app x, app y))
        in
        R.Same formatted_ar
      | R.New ar ->
        R.New (Array.map ar ~f:(apply ~refined:false ~rule:rules.Rz.line_new))
      | R.Old ar ->
        R.Old (Array.map ar ~f:(apply ~refined:false ~rule:rules.Rz.line_old))
      | R.Unified ar ->
        R.Unified (Array.map ar ~f:(apply ~refined:true ~rule:rules.Rz.line_unified))
      | R.Replace (ar1, ar2) ->
        let ar1 = Array.map ar1 ~f:(apply ~refined:true ~rule:rules.Rz.line_old) in
        let ar2 = Array.map ar2 ~f:(apply ~refined:true ~rule:rules.Rz.line_new) in
        R.Replace (ar1, ar2)

    let map_ranges hunks ~f =
      let f hunk =
        let module H = Patience_diff.Hunk in
        { hunk with
          H.ranges = List.map hunk.H.ranges ~f;
        }
      in
      List.map hunks ~f

    let apply hunks ~rules ~output =
      map_ranges hunks ~f:(to_string rules output)

  end

  let print ?file_names ~rules ~output ~print hunks =
    let formatted_hunks = Rules.apply ~rules ~output hunks in
    match output with
    | Output.Ansi -> Ansi.print ?file_names ~rules ~print formatted_hunks
    | Output.Html -> Html.print ?file_names ~rules ~print formatted_hunks
  ;;
end

(* Strip whitespace from a string by stripping and replacing with spaces *)
let ws_rex = Pcre.regexp "[\\s]+"
let ws_sub = Pcre.subst " "
let remove_ws s = String.strip (Pcre.replace ~rex:ws_rex ~itempl:ws_sub s)

let diff ~context ~compare ~keep_ws ~mine ~other =
  if keep_ws then
    let transform = fun x -> x in
    Patience_diff.get_hunks ~mine ~other ~transform ~context ~compare
  else
    let compare = fun x y -> compare (remove_ws x)  (remove_ws y) in
    let transform = remove_ws in
    Patience_diff.get_hunks ~mine ~other ~transform ~context ~compare

(* This regular expression describes the delimiters on which to split the string *)
let words_rex = Pcre.regexp
  "\\\"|\\{|\\}|\\[|\\]|[\\=\\`\\+\\-\\/\\!\\@\\$\\%\\^\\&\\*\\:]+|\\#|,|\\.|;|\\)|\\(|\\s+"

(* Split a string into a list of string options delimited by words_rex
   (delimiters included) *)
let split s ~keep_ws =
  let s = if keep_ws then s else String.rstrip s in
  let list = Pcre.full_split ~rex:words_rex s in
  List.filter_map list ~f:
    (fun split_result ->
      match split_result with
      | Pcre.Text s -> Some s
      | Pcre.Delim s -> Some s
      | Pcre.Group _ -> assert false   (* Irrelevant, since rex has no groups *)
      | Pcre.NoGroup -> assert false ) (* Ditto *)

(* Splits an array of lines into an array of pieces (`Newlines and R.Words) *)
let explode ar ~keep_ws =
  let words = Array.to_list ar in
  let words = List.map words ~f:(split ~keep_ws) in
  let to_words l = List.map l ~f:(fun s -> `Word s) in
  let words = List.concat_map words
    ~f:(fun x -> match x with
    | hd :: tl ->
      if (Pcre.pmatch ~rex:ws_rex hd) then
        `Newline (1, (Some hd)) :: (to_words tl)
      else
        `Newline (1, None) :: `Word hd :: (to_words tl)
    | [] -> [`Newline (1, None)]) in
  let words = List.fold_right words ~init:[]
    ~f:(fun x acc -> match acc with
    | `Word s :: tl -> x :: `Word s :: tl
    | `Newline (i, None) :: tl -> begin match x with
      | `Word s -> `Word s :: `Newline (i, None) :: tl
      | `Newline (j, opt) -> `Newline (i+j, opt) :: tl end
    | `Newline (i, Some s1) :: tl -> begin match x with
      | `Word s2 -> `Word s2 :: `Newline (i, Some s1) :: tl
      | `Newline (j, opt) ->
        let s1 = (Option.value opt ~default:"") ^ s1 in
        `Newline (i+j, Some s1) :: tl end
    | [] -> [x]) in
  (* Throw away the very first `Newline *)
  let words = match words with
    | `Newline (_i, opt) :: tl -> `Newline (0, opt) :: tl
    | `Word _ :: _ | [] -> assert false (* hd is always `Newline *)
  in
  (* Append a newline to the end, if this array has any words *)
  let words = match words with
    | [] -> []
    | [`Newline (0, None)] -> []
    | list -> List.append list [`Newline (1, None)] in
  Array.of_list words

(* Takes hunks of `Words and `Newlines and collapses them back into lines,
 * formatting appropriately. *)
let collapse ranges ~rule_same ~rule_old ~rule_new ~kind ~output =
  let module H = Patience_diff.Hunk in
  let module R = Patience_diff.Range in
  (* flag indicates what kind of range is currently being collapsed *)
  let flag = ref `Same in
  (* segment is the current series of words being processed. *)
  let segment = ref [] in
  (* line is the current series of formatted segments *)
  let line = ref [] in
  (* lines is the return array *)
  let lines = ref [] in
  let apply = Output_ops.Rule.apply ~output ~refined:false in
  (*
   * Finish the current segment by applying the appropriate format
   * and popping it on to the end of the current line
   *)
  let finish_segment () =
    let rule = match !flag with
      | `Same -> rule_same
      | `Old -> rule_old
      | `New -> rule_new in
    let formatted_segment = List.rev !segment |! String.concat |! apply ~rule in
    line := formatted_segment :: !line ;
    segment := [] in
  (*
   * Finish the current segment, apply the reset rule to the line,
   * and pop the finished line onto the return array
   *)
  let newline i =
    for _i = 1 to i do
      finish_segment ();
      lines := (String.concat (List.rev !line)) :: !lines;
      line := []
    done in
  let f range =
    (* Extract the array, set flag appropriately, *)
    let ar = match range with
      | R.Same ar ->
        flag := `Same;
        (* R.Same ar is an array of tuples.  The first tuple is an
         * element from the old file, the second tuple, an element
         * from the new file.  Depending on what kind of collapse
         * this is, we want only one or the other. *)
        let f = match kind with
          | `Old_only -> fst
          | `New_only -> snd
          | `Unified -> snd in
        Array.map ar ~f
      | R.Old ar ->
        flag := `Old;
        ar
      | R.New ar ->
        flag := `New;
        ar
      | R.Replace _ | R.Unified _ ->
        (* When calling collapse, we always call
         * Patience_diff.unified first, which removes all R.Replaces
         * and R.Unifieds. *)
        assert false in
    (* Iterate through the elements of the range, appending each `Word to
     * segment and calling newline on each `Newline
     *)
    Array.iter ar ~f:(function
    | `Newline (i, None) -> newline i;
    | `Newline (i, Some s) -> newline i; segment := s :: !segment
    | `Word s -> segment := s :: !segment);
    finish_segment ()
  in
  List.iter ranges ~f;
  Array.of_list (List.rev !lines)

let piece_transform = function
  | `Word s -> s
  | `Newline _ -> " "

(* Get the hunks from two arrays of pieces (`Words and `Newlines) *)
let diff_pieces ~old_pieces ~new_pieces ~keep_ws =
  let context = -1 in
  let transform =
    if keep_ws then piece_transform
    else fun x -> remove_ws (piece_transform x) in
  let compare = String.compare in
  Patience_diff.get_hunks ~mine:old_pieces ~other:new_pieces ~transform ~context ~compare

let ranges_are_just_whitespace ranges =
  let module R = Patience_diff.Range in
  List.for_all ranges ~f:(function
  | R.Old piece_array | R.New piece_array ->
    Array.for_all piece_array ~f:(fun piece ->
      let s = piece_transform piece in
      remove_ws s = "")
  | _ -> true)

(* Refines the diff, splitting the lines into smaller arrays and diffing them, then
   collapsing them back into their initial lines after applying a format. *)
let refine ~rules ~produce_unified_lines ~output ~keep_ws ~split_long_lines hunks =
  let module R = Patience_diff.Range in
  let module H = Patience_diff.Hunk in
  let module Rz = Format.Rules in
  let rule_old = rules.Rz.word_old in
  let rule_new = rules.Rz.word_new in
  let collapse = collapse ~rule_old ~rule_new ~output in

  let console_width =
    Memo.unit (fun () ->
      assert split_long_lines;
      match Linux_ext.get_terminal_size with
      | Error _ -> 80
      | Ok get_size -> snd (get_size ())
    )
  in
  let aux hunk =
    let aux = function
      | R.Replace (old_ar, new_ar) ->
        (* Explode the arrays *)
        let old_pieces = explode old_ar ~keep_ws in
        let new_pieces = explode new_ar ~keep_ws in
        (* Diff the pieces *)
        let sub_diff = diff_pieces ~old_pieces ~new_pieces ~keep_ws in

        (* Smash the hunks' ranges all together *)
        let sub_diff = Patience_diff.ranges sub_diff in

        (* Break it up where lines are too long *)
        let sub_diff_pieces =
          if not split_long_lines
          then [sub_diff]
          else
            let max_len = Int.max 20 (console_width () - 2) in

            (* Accumulates the total length of the line so far, summing lengths
               of word tokens but resetting when newlines are hit *)
            let get_new_len_so_far ~len_so_far tokens_arr =
              Array.fold ~init:len_so_far tokens_arr ~f:(fun len_so_far token ->
                match token with
                | `Newline _ -> 0
                | `Word word -> len_so_far + String.length word
              )
            in

            (* Iteratively split long lines up.
               Produces a list of "range lists", where each range list should be displayed
               all together in one unbroken piece before being followed by the next range
               list, etc. *)
            let rec split_lines len_so_far sub_diff rangeaccum rangelistaccum =
              match sub_diff with
              | [] -> begin
                match rangeaccum with
                | [] -> List.rev rangelistaccum
                | _ -> List.rev (List.rev rangeaccum :: rangelistaccum)
              end
              (* More tokens ranges left to process *)
              | range :: rest ->
                match range with
                | R.Same tokenpairs_arr ->

                  let range_of_tokens tokenpairs =
                    R.Same (Array.of_list tokenpairs)
                  in

                  (* Keep taking tokens until we exceed max_len or hit a newline.
                     Returns (new len_so_far, new range, remaining tokens)*)
                  let rec take_until_max len_so_far tokenpairs accum =
                    match tokenpairs with
                    | [] -> (len_so_far, range_of_tokens (List.rev accum), [])
                    | ((token,_) as tokenpair) :: rest ->
                      match token with
                      | `Newline _ ->
                        (0, range_of_tokens (List.rev (tokenpair :: accum)), rest)
                      | `Word word ->
                        let wordlen = String.length word in
                        if wordlen + len_so_far > max_len && len_so_far > 0
                        then (0, range_of_tokens (List.rev accum), tokenpairs)
                        else take_until_max (wordlen + len_so_far) rest (tokenpair :: accum)
                  in

                  let make_newline () =
                    R.Same [|`Newline (1,None), `Newline (1,None)|]
                  in

                  (* Keep taking ranges until all tokens exhausted.
                     Returns (new len_so_far, range list) *)
                  let rec take_ranges_until_exhausted len_so_far tokenpairs accum =
                    match tokenpairs with
                    | [] -> (len_so_far, List.rev accum)
                    | _ ->
                      let new_len_so_far, new_range, new_tokenpairs =
                        take_until_max len_so_far tokenpairs []
                      in
                      let new_accum = `Range new_range :: accum in
                      (* If there are token pairs left, that means we hit the max_len,
                         so add a break at this point *)
                      let new_accum =
                        if new_tokenpairs <> []
                        then `Break :: `Range (make_newline ()) :: new_accum
                        else new_accum
                      in
                      take_ranges_until_exhausted new_len_so_far new_tokenpairs new_accum
                  in
                  let new_len_so_far, new_ranges =
                    take_ranges_until_exhausted len_so_far (Array.to_list tokenpairs_arr) []
                  in

                  (* Update rangeaccum and rangelistaccum according to the `Ranges and
                     `Breaks. `Ranges accumulate on to the existing range list to be
                     displayed contiguously, `Breaks start a new range list. *)
                  let (rangeaccum, rangelistaccum) =
                    List.fold new_ranges ~init:(rangeaccum, rangelistaccum)
                      ~f:(fun (rangeaccum, rangelistaccum) r ->
                        match r with
                        | `Break -> ([], List.rev rangeaccum :: rangelistaccum)
                        | `Range r -> (r :: rangeaccum, rangelistaccum)
                      )
                  in
                  split_lines new_len_so_far rest rangeaccum rangelistaccum
                | R.New tokens_arr
                | R.Old tokens_arr ->
                  let new_len_so_far = get_new_len_so_far ~len_so_far tokens_arr in
                  split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                | R.Replace (old_arr,new_arr) ->
                  let new_len_so_far = Int.max
                    (get_new_len_so_far ~len_so_far old_arr)
                    (get_new_len_so_far ~len_so_far new_arr)
                  in
                  split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                | R.Unified _ -> assert false
            in
            split_lines 0 sub_diff [] []
        in

        List.concat_map sub_diff_pieces ~f:(fun sub_diff ->
          let sub_old = Patience_diff.Range.old_only sub_diff in
          let sub_new = Patience_diff.Range.new_only sub_diff in
          let old_all_same = Patience_diff.Range.all_same sub_old in
          let new_all_same = Patience_diff.Range.all_same sub_new in

          let produce_unified_lines =
            produce_unified_lines &&
              ((not (ranges_are_just_whitespace sub_old) && new_all_same) ||
                  (not (ranges_are_just_whitespace sub_new) && old_all_same))
          in

          (* Collapse the pieces back into lines *)
          let old_new_pairs =
            begin match old_all_same, new_all_same with
            | true, true ->
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_unified in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ (new_ar, new_ar) ]
            | false, true ->
              let kind = `Old_only in
              let rule_same =
                if produce_unified_lines
                then rules.Rz.word_same_unified
                else rules.Rz.word_same_old
              in
              let old_ar = collapse sub_old ~rule_same ~kind in
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_new in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ (old_ar, new_ar) ]
            | true, false ->
              let kind = `New_only in
              let rule_same =
                if produce_unified_lines
                then rules.Rz.word_same_unified
                else rules.Rz.word_same_new
              in
              let new_ar = collapse sub_new ~rule_same ~kind in
              let kind = `Old_only in
              let rule_same = rules.Rz.word_same_old in
              let old_ar = collapse sub_old ~rule_same ~kind in
              [ (old_ar, new_ar) ]
            | false, false ->
              let kind = `Old_only in
              let rule_same = rules.Rz.word_same_old in
              let old_ar = collapse sub_old ~rule_same ~kind in
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_new in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ (old_ar, new_ar) ]
            end
          in

          List.map old_new_pairs ~f:(fun (old_ar, new_ar) ->
            let range =
              match old_all_same, new_all_same with
              | true, true -> R.Same (Array.map new_ar ~f:(fun x -> (x,x)))
              | _ -> match old_ar, new_ar with
                (* Ugly hack that takes care of empty files *)
                | [|""|], new_ar -> R.Replace ([||], new_ar)
                | old_ar, [|""|] -> R.Replace (old_ar, [||])
                | old_ar, new_ar ->
                  match produce_unified_lines, old_all_same, new_all_same with
                  | true, true, false -> R.Unified new_ar
                  | true, false, true -> R.Unified old_ar
                  | false, _, _ | _, false, false -> R.Replace (old_ar, new_ar)
                  | _ -> assert false
            in
            range
          )
        )
      | range -> [ range ]
    in
    let refined_ranges = List.concat_map hunk.H.ranges ~f:aux in
    { hunk with
      H.ranges = refined_ranges;
    }
  in
  let refined_hunks = List.map hunks ~f:aux in
  List.filter refined_hunks ~f:(fun h -> not (H.all_same h))
;;

let print ~old_file ~new_file ~rules ~output hunks =
  Output_ops.print
    ~rules ~output ~file_names:(old_file, new_file) ~print:(Printf.printf "%s\n")
    hunks
;;

let output_to_string ?file_names ~rules ~output hunks =
  let buf = Queue.create () in
  Output_ops.print ?file_names hunks ~rules ~output ~print:(Queue.enqueue buf);
  String.concat (Queue.to_list buf) ~sep:"\n"
;;

let iter_ansi ~rules ~f_hunk_break ~f_line hunks =
  let hunks = Output_ops.Rules.apply hunks ~rules ~output:Ansi in
  Ansi.iter ~f_hunk_break ~f_line hunks
;;
