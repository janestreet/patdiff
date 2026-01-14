open! Core
open! Import
include Output_intf

type t =
  | Ansi
  | Ascii
  | Html
[@@deriving compare ~localize, sexp]

let implies_unrefined t =
  match t with
  | Ansi | Html -> false
  | Ascii -> true
;;

let flag =
  let open Command.Param in
  choose_one
    [ map
        ~f:(function
          | true -> Some (Some Html)
          | _ -> None)
        (flag
           "html"
           no_arg
           ~doc:" Output in HTML format instead of default (ASCII with ANSI escape codes)")
    ; map
        ~f:(function
          | true -> Some (Some Ascii)
          | _ -> None)
        (flag
           "ascii"
           no_arg
           ~doc:" Output in ASCII with no ANSI escape codes (implies unrefined diff)")
    ; map
        ~f:(function
          | true -> Some (Some Ansi)
          | _ -> None)
        (flag "ansi" no_arg ~doc:" Output in ASCII with ANSI escape codes")
    ]
    ~if_nothing_chosen:(Default_to None)
;;
