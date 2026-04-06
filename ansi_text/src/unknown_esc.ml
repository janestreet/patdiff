open! Core

type t =
  | Csi of string
  | Fe of char
  | Fp of char
  | Nf of string
  | Incomplete
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(* Custom quickcheck generator that produces valid escape sequences *)
let quickcheck_generator =
  let open Base_quickcheck.Generator in
  let param_char = of_list (List.init 16 ~f:(fun i -> Char.of_int_exn (0x30 + i))) in
  let csi_final_char =
    (* Exclude 'm' (style), A-H/J/K/S/T (control), 'n' (DSR), and 'h'/'l' (private mode)
       to make these truly "unknown" *)
    of_list
      (List.filter
         (List.init 63 ~f:(fun i -> Char.of_int_exn (0x40 + i)))
         ~f:(fun c ->
           not
             (Char.equal c 'm'
              || Char.equal c 'n'
              || Char.equal c 'h'
              || Char.equal c 'l'
              || Char.equal c 'A'
              || Char.equal c 'B'
              || Char.equal c 'C'
              || Char.equal c 'D'
              || Char.equal c 'E'
              || Char.equal c 'F'
              || Char.equal c 'G'
              || Char.equal c 'H'
              || Char.equal c 'J'
              || Char.equal c 'K'
              || Char.equal c 'S'
              || Char.equal c 'T')))
  in
  let csi_gen =
    (* Generate valid CSI: parameter bytes (0x30-0x3F)* + final byte (0x40-0x7E) *)
    both (string_of param_char) csi_final_char
    |> map ~f:(fun (params, final) -> Csi (params ^ String.make 1 final))
  in
  let fe_gen =
    (* Fe bytes: 0x40-0x5F excluding '[' (0x5B) and ']' (0x5D) *)
    of_list
      (List.filter
         (List.init 32 ~f:(fun i -> Char.of_int_exn (0x40 + i)))
         ~f:(fun c -> not (Char.equal c '[' || Char.equal c ']')))
    |> map ~f:(fun c -> Fe c)
  in
  let fp_gen =
    (* Fp bytes: 0x60-0x7E *)
    of_list (List.init 31 ~f:(fun i -> Char.of_int_exn (0x60 + i)))
    |> map ~f:(fun c -> Fp c)
  in
  let nf_gen =
    (* nF: one or more intermediate bytes (0x20-0x2F) + final byte (0x30-0x7E) *)
    let intermediate_char =
      of_list (List.init 16 ~f:(fun i -> Char.of_int_exn (0x20 + i)))
    in
    let final_char = of_list (List.init 79 ~f:(fun i -> Char.of_int_exn (0x30 + i))) in
    both (string_non_empty_of intermediate_char) final_char
    |> map ~f:(fun (intermediates, final) -> Nf (intermediates ^ String.make 1 final))
  in
  union [ csi_gen; fe_gen; fp_gen; nf_gen; return Incomplete ]
;;

let of_csi ?private_prefix params ~terminal =
  match private_prefix with
  | None -> Csi (sprintf "%s%c" params terminal)
  | Some prefix -> Csi (sprintf "%c%s%c" prefix params terminal)
;;

let to_string = function
  | Csi params -> "\027[" ^ params
  | Fe c -> sprintf "\027%c" c
  | Fp c -> sprintf "\027%c" c
  | Nf s -> "\027" ^ s
  | Incomplete -> "\027"
;;

let to_string_hum = function
  | Csi params -> sprintf "(ANSI-CSI:%s)" params
  | Fe c -> sprintf "(ANSI-Fe:%s)" (Char.escaped c)
  | Fp c -> sprintf "(ANSI-Fp:%s)" (Char.escaped c)
  | Nf s -> sprintf "(ANSI-nF:%s)" (String.escaped s)
  | Incomplete -> "(ESC)"
;;
