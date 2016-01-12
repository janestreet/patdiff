open! Core.Std
open! Import

type t =
  | Ansi
  | Ascii
[@@deriving sexp]

let implies_unrefined t =
  match t with
  | Ansi  -> false
  | Ascii -> true
;;
