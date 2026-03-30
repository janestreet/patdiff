open! Core

type action =
  | Set
  | Reset
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t =
  { modes : int list
  ; action : action
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let quickcheck_generator =
  let open Base_quickcheck.Generator.Let_syntax in
  let%bind modes = List.quickcheck_generator (Int.gen_incl 1 9999) in
  let modes = if List.is_empty modes then [ 1 ] else modes in
  let%bind action = [%quickcheck.generator: action] in
  return { modes; action }
;;

let of_csi_params params ~terminal =
  let action =
    match terminal with
    | 'h' -> Some Set
    | 'l' -> Some Reset
    | _ -> None
  in
  let modes = List.filter_opt params in
  match action with
  | Some action when not (List.is_empty modes) -> Some { modes; action }
  | _ -> None
;;

let alternate_screen_modes = [ 47; 1049 ]

let is_alternate_screen t =
  List.exists t.modes ~f:(fun mode ->
    List.mem alternate_screen_modes mode ~equal:Int.equal)
;;

let to_string { modes; action } =
  let modes_str = List.map modes ~f:Int.to_string |> String.concat ~sep:";" in
  let action_char =
    match action with
    | Set -> 'h'
    | Reset -> 'l'
  in
  sprintf "\027[?%s%c" modes_str action_char
;;

let to_string_hum { modes; action } =
  let modes_str = List.map modes ~f:Int.to_string |> String.concat ~sep:";" in
  let action_str =
    match action with
    | Set -> "set"
    | Reset -> "reset"
  in
  sprintf "(PrivateMode:%s:%s)" action_str modes_str
;;
