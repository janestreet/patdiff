open! Core
open Angstrom
open Angstrom.Let_syntax

(* NOTE: This was largely AI-generated, with oversight and review from Bryce. *)

(* This parser implements a subset of ECMA-48 (also known as ANSI X3.64 or ISO/IEC 6429).

   Key sections referenced:
   - Section 5.4: Control sequences (CSI)
   - Section 8.3.143: ST (String Terminator)
   - Section 8.3.89: OSC (Operating System Command)

   For OSC 8 hyperlinks specifically, see:
   https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda

   Design note: Each parser returns a list of elements to handle malformed sequences
   efficiently. When CSI or OSC parsing fails, we return [Escape (Some '[' or ']')]
   without consuming anything beyond that byte, allowing the rest to be parsed normally.
   This avoids backtracking and expensive peeking of the entire input. *)

let esc_char = '\027'
let bel_char = '\007'

(* === Character predicates for CSI sequences (ECMA-48 section 5.4) === *)

(* Parameter bytes: 0x30-0x3F (ECMA-48 section 5.4.2a). Includes digits and semicolons
   used by SGR and recognized by [Ansi_text], plus a few other characters we ignore. *)
let is_csi_param_byte c =
  let code = Char.to_int c in
  0x30 <= code && code <= 0x3F
;;

(* Intermediate bytes: 0x20-0x2F (ECMA-48 section 5.4.2b). Rarely used in practice, and
   ignored by [Ansi_text]. *)
let is_csi_intermediate_byte c =
  let code = Char.to_int c in
  0x20 <= code && code <= 0x2F
;;

(* Final byte: 0x40-0x7E per ECMA-48 section 5.4.2c *)
let is_csi_final_byte c =
  let code = Char.to_int c in
  0x40 <= code && code <= 0x7E
;;

let esc = char esc_char

let text_of_trailing str =
  if String.is_empty str then [] else [ `Text (Text.of_string str) ]
;;

(* ECMA-48 section 5.4: CSI is [{ ESC [ }] (0x1B 0x5B)
   Format: CSI P...P I...I F
   where P = parameter bytes, I = intermediate bytes, F = final byte

   Returns a list to handle malformed sequences: valid CSI returns [element],
   malformed returns [Escape (Some '[')] without consuming params/intermediate. *)
let csi =
  let%bind params = char '[' *> take_while is_csi_param_byte
  and intermediate = take_while is_csi_intermediate_byte in
  match%bind peek_char with
  | Some c when is_csi_final_byte c ->
    (* Valid CSI: consume final byte and parse *)
    any_char
    >>| fun terminal -> [ (Ansi.of_csi ~params ~terminal :> Text_with_ansi.element) ]
  | _ ->
    (* Malformed CSI: no valid final byte. Return Fe '[' followed by
       params and intermediate as text, so nothing is lost. *)
    return (`Unknown (Unknown_esc.Fe '[') :: text_of_trailing (params ^ intermediate))
;;

(*= OSC 8 is the hyperlink extension (not part of ECMA-48): Format:
   [{ OSC 8 ; params ; uri ST}]
   See: https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
   Returns a list to handle malformed sequences. *)
let osc_hyperlink_of_payload payload =
  match String.chop_prefix payload ~prefix:"8;" with
  | None -> None
  | Some rest ->
    (* Find the next semicolon to separate params from URI *)
    (match String.lsplit2 rest ~on:';' with
     | None -> None
     | Some (_params, uri) ->
       (* Empty URI means end of hyperlink *)
       if String.is_empty uri then Some Hyperlink.End else Some (Hyperlink.Start uri))
;;

(* ECMA-48 section 8.3.89: OSC is [{ ESC ] }] (0x1B 0x5D)
   Format: OSC <payload> ST
   where ST (String Terminator) is [{ ESC \ }] (0x1B 0x5C) or BEL (0x07)

   Note: While ECMA-48 only defines ESC \ as the String Terminator, BEL is widely
   accepted as an alternative terminator for OSC sequences in real-world terminals
   (xterm, iTerm2, etc.). This is for backwards compatibility with older terminals. *)
let is_osc_terminator c = Char.equal c esc_char || Char.equal c bel_char

let osc =
  let%bind payload = char ']' *> take_till is_osc_terminator in
  match%bind peek_char with
  | Some c when Char.equal c bel_char ->
    (* BEL terminator: consume it and interpret the OSC *)
    let%map () = advance 1 in
    (match osc_hyperlink_of_payload payload with
     | Some hyperlink -> [ `Hyperlink hyperlink ]
     | None -> [ `Unknown (Unknown_esc.Osc payload) ])
  | Some _ ->
    (* Must be ESC - check if it's ESC \ *)
    let%bind available in
    if available >= 2
    then (
      match%bind peek_string 2 with
      | "\027\\" ->
        (* Valid ST: consume both characters and interpret the OSC *)
        let%map () = advance 2 in
        (match osc_hyperlink_of_payload payload with
         | Some hyperlink -> [ `Hyperlink hyperlink ]
         | None -> [ `Unknown (Unknown_esc.Osc payload) ])
      | _ ->
        (* ESC followed by something other than backslash - unterminated OSC. Return
           Fe ']' followed by payload as text. Don't consume the ESC. *)
        return (`Unknown (Unknown_esc.Fe ']') :: text_of_trailing payload))
    else
      (* Not enough input for ST - unterminated OSC at end of input *)
      return (`Unknown (Unknown_esc.Fe ']') :: text_of_trailing payload)
  | None ->
    (* End of input - unterminated OSC *)
    return (`Unknown (Unknown_esc.Fe ']') :: text_of_trailing payload)
;;

(* Fe escape sequences: ESC + byte in 0x40-0x5F per ECMA-48 section 5.3. Note:
   '[' (0x5B) and ']' (0x5D) are in this range but CSI and OSC are handled separately. *)
let is_fe_byte c =
  let code = Char.to_int c in
  0x40 <= code && code <= 0x5F
;;

(* Fp escape sequences: ESC + byte in 0x60-0x7E per ECMA-48 section 5.3. *)
let is_fp_byte c =
  let code = Char.to_int c in
  0x60 <= code && code <= 0x7E
;;

(* Intermediate bytes for nF escape sequences: 0x20-0x2F per ECMA-48 section 5.4.2b *)
let is_nf_intermediate_byte c =
  let code = Char.to_int c in
  0x20 <= code && code <= 0x2F
;;

(* Final byte for nF escape sequences: 0x30-0x7E per ECMA-48 section 5.4.2c *)
let is_nf_final_byte c =
  let code = Char.to_int c in
  0x30 <= code && code <= 0x7E
;;

(* Handles ESC followed by any byte that isn't '[' (CSI) or ']' (OSC).
   - Fe sequences (0x40-0x5F, excluding '[' and ']')
   - Fp sequences (0x60-0x7E): private use, includes common sequences like ESC c (reset)
   - nF sequences (0x20-0x2F intermediate bytes + 0x30-0x7E final byte)
   - Incomplete: ESC at end of input, ESC+ESC, or ESC + invalid byte *)
let other_escape =
  match%bind peek_char with
  | Some '\027' | None -> return [ `Unknown Unknown_esc.Incomplete ]
  | Some escaped ->
    if is_fe_byte escaped
    then (
      (* '[' and ']' already handled by caller *)
      let%map () = advance 1 in
      [ `Unknown (Unknown_esc.Fe escaped) ])
    else if is_fp_byte escaped
    then (
      let%map () = advance 1 in
      [ `Unknown (Unknown_esc.Fp escaped) ])
    else if is_nf_intermediate_byte escaped
    then (
      (* consume intermediate bytes + final byte *)
      let%bind intermediates = take_while1 is_nf_intermediate_byte in
      match%bind peek_char with
      | Some final when is_nf_final_byte final ->
        let%map () = advance 1 in
        [ `Unknown (Unknown_esc.Nf (intermediates ^ String.make 1 final)) ]
      | _ ->
        (* Truncated nF: emit Incomplete, leave intermediates as text *)
        return (`Unknown Unknown_esc.Incomplete :: text_of_trailing intermediates))
    else
      (* Invalid byte - emit Incomplete, leave byte for text parsing *)
      return [ `Unknown Unknown_esc.Incomplete ]
;;

(*= Main escape sequence dispatcher. After ESC, the next byte determines sequence type:
   - '[' (0x5B) -> CSI (Control Sequence Introducer)
   - ']' (0x5D) -> OSC (Operating System Command)
   - other -> Other escape sequence *)
let escape_sequence =
  match%bind esc *> peek_char with
  | Some '[' -> csi (* CSI: ESC [ *)
  | Some ']' -> osc (* OSC: ESC ] *)
  | Some _ -> other_escape
  | None ->
    (* ESC at end of input *)
    return [ `Unknown Unknown_esc.Incomplete ]
;;

let plain_text =
  let%map s = take_while1 (fun c -> not (Char.equal c esc_char)) in
  [ `Text (Text.of_string s) ]
;;

let ( => ) peek_for case =
  match%bind peek_string (String.length peek_for) with
  | peeked when String.equal peeked peek_for -> return case
  | _ -> Angstrom.fail "This message should not reach the top-level. Seeing it is a bug!"
;;

let parser =
  let%map out =
    fix (fun parser ->
      match%bind peek_char with
      | None -> return Appendable_list.empty
      | Some _ ->
        let%map curr =
          match%bind choice [ "\027" => `Escape; return `Plain_text ] with
          | `Escape -> escape_sequence
          | `Plain_text -> plain_text
        and rest = parser in
        Appendable_list.append (Appendable_list.of_list curr) rest)
  in
  Appendable_list.to_list out
;;

let parse str =
  match parse_string ~consume:All parser str with
  | Ok result -> result
  | Error _ ->
    (* Should be unreachable, but fallback just in case *)
    [ `Text (Text.of_string str) ]
;;
