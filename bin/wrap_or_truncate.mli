type t =
  [ `wrap
  | `truncate
  ]
[@@deriving enumerate]

val to_string : t -> string
