open! Core

type command =
  | Set_title of string
  | Set_icon_name of string
  | Set_working_directory of string
  | Other of string
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t = { command : command }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let quickcheck_generator =
  let open Base_quickcheck.Generator in
  (* We include only printable chars and explictly ignore ESC and BEL. *)
  let safe_char =
    Char.quickcheck_generator
    |> filter ~f:(fun c ->
      let code = Char.to_int c in
      code >= 0x20
      && code <= 0x7E
      && (not (Char.equal c '\027'))
      && not (Char.equal c '\007'))
  in
  let safe_string = string_of safe_char in
  union
    [ map safe_string ~f:(fun s -> { command = Set_title s })
    ; map safe_string ~f:(fun s -> { command = Set_icon_name s })
    ; map safe_string ~f:(fun s ->
        { command = Set_working_directory ("file://localhost" ^ s) })
    ; map safe_string ~f:(fun s -> { command = Other s })
    ]
;;

let of_payload payload =
  (* Parse the OSC command number and payload *)
  match String.lsplit2 payload ~on:';' with
  | Some ("0", title) -> { command = Set_title title }
  | Some ("2", title) -> { command = Set_title title }
  | Some ("1", name) -> { command = Set_icon_name name }
  | Some ("7", uri) -> { command = Set_working_directory uri }
  | _ -> { command = Other payload }
;;

let payload_of_command = function
  | Set_title title -> "0;" ^ title
  | Set_icon_name name -> "1;" ^ name
  | Set_working_directory uri -> "7;" ^ uri
  | Other payload -> payload
;;

let to_string { command } =
  let payload = payload_of_command command in
  (* Use ESC \ as the terminator for consistency *)
  "\027]" ^ payload ^ "\027\\"
;;

let to_string_hum { command } =
  match command with
  | Set_title title -> sprintf "(OSC:title:%s)" title
  | Set_icon_name name -> sprintf "(OSC:icon:%s)" name
  | Set_working_directory uri -> sprintf "(OSC:cwd:%s)" uri
  | Other payload -> sprintf "(OSC:%s)" payload
;;
