open! Core

type query =
  | Device_status
  | Cursor_position
  | Other of int
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t = { query : query }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let quickcheck_generator =
  let open Base_quickcheck.Generator in
  let query_gen =
    union
      [ return Device_status
      ; return Cursor_position
      ; map (Int.gen_incl 0 99) ~f:(fun n -> Other n)
      ]
  in
  map query_gen ~f:(fun query -> { query })
;;

let of_params = function
  | [ Some 5 ] -> Some { query = Device_status }
  | [ Some 6 ] -> Some { query = Cursor_position }
  | [ Some n ] -> Some { query = Other n }
  | _ -> None
;;

let query_to_param = function
  | Device_status -> 5
  | Cursor_position -> 6
  | Other n -> n
;;

let to_string { query } = sprintf "\027[%dn" (query_to_param query)

let to_string_hum { query } =
  match query with
  | Device_status -> "(DSR:device-status)"
  | Cursor_position -> "(DSR:cursor-position)"
  | Other n -> sprintf "(DSR:%d)" n
;;
