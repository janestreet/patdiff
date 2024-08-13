type t =
  [ `wrap
  | `truncate
  ]
[@@deriving enumerate]

let to_string = function
  | `wrap -> "wrap"
  | `truncate -> "truncate"
;;
