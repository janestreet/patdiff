open! Core
open! Async
open Expect_test_helpers

let pipe commands =
  List.map commands ~f:(fun (prog, args) ->
    String.concat ~sep:" " (List.map (prog :: args) ~f:Filename.quote))
  |> String.concat ~sep:" | "
  |> sprintf "set -o pipefail; %s"
  |> system
;;

let patdiff ~mine ~other extra_flags = with_temp_dir (fun dir ->
  let%bind () = Writer.save (dir ^/ "mine") ~contents:mine
  and      () = Writer.save (dir ^/ "other") ~contents:other
  in
  pipe [ ("../bin/patdiff.exe"
         , [ "-default"
           ; "-alt-old"; "mine"
           ; "-alt-new"; "other"
           ; dir ^/ "mine"
           ; dir ^/ "other"
           ] @ extra_flags)
       ; ("./visible-colors.sh", [])
       ])
;;

let ahello = "
  hello
  hello
  hello
  goodbye
"

let bhello = "
  goodbye
  hello
  hello
  hello
"

let%expect_test "Don't focus too much on unique lines if it gives absurd diffs." =
  let%bind () = patdiff ~mine:ahello ~other:bhello [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,5 +1,5(off) ============================================================
        (fg:black) |
        (fg:black bg:red)-|(fg:red)  hello
        (fg:black bg:red)-|(fg:red)  hello
        (fg:black bg:red)-|(fg:red)  hello
        (fg:black) |(off)  goodbye
        (fg:black bg:green)+|(fg:green)  hello
        (fg:black bg:green)+|(fg:green)  hello
        (fg:black bg:green)+|(fg:green)  hello
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;


let ayellow = "
val kernel                         : string option
val build_date                     : Date.t option
val build_time                     : Time_float.Ofday.t option
val x_library_inlining             : bool
"

let byellow = "
val kernel                         : string option
val build_time                     : Time_float.t option
val x_library_inlining             : bool
"

let%expect_test "Prefer one yellow line and one red line to two yellow lines" =
  let%bind () = patdiff ~mine:ayellow ~other:byellow [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,5 +1,4(off) ============================================================
        (fg:black) |
        (fg:black) |(off)val kernel                         : string option
        (fg:black bg:yellow)!|(off)val (fg:red)build_date                     : Date.t option
        (fg:black bg:yellow)!|(fg:red)val (off)build_time                     : Time_float.(fg:red)Ofday.(off)t option
        (fg:black) |(off)val x_library_inlining             : bool
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let acity = "hello and please remove me; hello with the signs of the city\n"

let bcity = "hello with the signs of the city\n"

let%expect_test "Prefer one block of equality to two" =
  let%bind () = patdiff ~mine:acity ~other:bcity [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
        (fg:black bg:yellow)!|(off)hello (fg:red)and please remove me; hello (off)with the signs of the city
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let aarb = "
          (the_army_that_fun_what_me_key_keyboard     (2000wel then ~1e-875))
          (the_army_that_fun_what_you_key_keyboard (1000wel then ~1e-730))
"

let barb = "
          (the_army_that_fun_what_me_key_keyboard     (3000wel then ~1e-589))
          (the_army_that_fun_what_you_key_keyboard (2000wel then ~1e-314))
"

let%expect_test "Try to match up long variable names" =
  let%bind () = patdiff ~mine:aarb ~other:barb [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,3 +1,3(off) ============================================================
        (fg:black) |
        (fg:black bg:red)-|(off)          (the_army_that_fun_what_me_key_keyboard     (2000wel then ~1e-(fg:red)875))
        (fg:black bg:red)-|(fg:red)          (the_army_that_fun_what_you_key_keyboard (1000wel then ~1e-730(off)))
        (fg:black bg:green)+|(off)          (the_army_that_fun_what_me_key_keyboard     ((fg:green)3000wel then ~1e-589))
        (fg:black bg:green)+|(fg:green)          (the_army_that_fun_what_you_key_keyboard ((off)2000wel then ~1e-(fg:green)314(off)))
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

(* The following section contains 3 examples from real diffs
   (i.e., different versions of the same Jane Street file). *)

let acode = "  let seen = Node_id.Hash_set.create () in
  let rec iter_descendants node =
    if not (Hash_set.mem seen node.id) then begin
      Hash_set.add seen node.id;
      f node;
      Node.iteri_children node ~f:(fun _ node -> iter_descendants node);
    end;
  in
  iter_observers t ~f:(fun (Internal_observer.Packed.T internal_observer) ->
    iter_descendants (Node.pack internal_observer.observing));
;;

let save_dot t file =
  Out_channel.with_file file ~f:(fun out ->
    let node_name node = \"n\" ^ Node_id.to_string node.id in
    fprintf out \"digraph G {\\n\";
    fprintf out \"  rankdir = BT\\n\";
    let handle_descendant (type a) (from : a Node.t) =
      let from_name = node_name from in
      fprintf out \"  %s [label=\"%s %s\\nheight = %d\"]\\n\"
        from_name from_name (Kind.name from.kind) from.height;
      Node.iteri_parents from ~f:(fun _ to_ ->
        fprintf out \"  %s -> %s\\n\" from_name (node_name to_));
      begin match from.kind with
      | Bind_lhs_change bind ->
        Bind.iter_nodes_created_on_rhs bind ~f:(fun to_ ->
          fprintf out \"  %s -> %s [style=dashed]\\n\" from_name (node_name to_));
      | _ -> ()
      end
    in
    iter_observer_descendants t ~f:handle_descendant;
    fprintf out \"}\\n%!\")
"
let bcode = "  Node.Packed.iter_descendants (directly_observed t) ~f\n"

let%expect_test "Delete spurious matches, part 1" =
  let%bind () = patdiff ~mine:acode ~other:bcode [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,32 +1,1(off) ============================================================
        (fg:black bg:red)-|(off)  (fg:red)let seen = Node_id.Hash_set.create () in
        (fg:black bg:red)-|(fg:red)  let rec iter_descendants node =
        (fg:black bg:red)-|(fg:red)    if not (Hash_set.mem seen node.id) then begin
        (fg:black bg:red)-|(fg:red)      Hash_set.add seen node.id;
        (fg:black bg:red)-|(fg:red)      f node;
        (fg:black bg:red)-|(fg:red)      (off)Node.(fg:red)iteri_children node ~f:(fun _ node -> iter_descendants node);
        (fg:black bg:red)-|(fg:red)    end;
        (fg:black bg:red)-|(fg:red)  in
        (fg:black bg:red)-|(fg:red)  iter_observers t ~f:(fun (Internal_observer.(off)Packed.(fg:red)T internal_observer) ->
        (fg:black bg:red)-|(fg:red)    (off)iter_descendants ((fg:red)Node.pack(off) (fg:red)internal_observer.observing));
        (fg:black bg:red)-|(fg:red);;
        (fg:black bg:red)-|
        (fg:black bg:red)-|(fg:red)let save_dot t file =
        (fg:black bg:red)-|(fg:red)  Out_channel.with_file file ~f:(fun out ->
        (fg:black bg:red)-|(fg:red)    let node_name node = "n" ^ Node_id.to_string node.id in
        (fg:black bg:red)-|(fg:red)    fprintf out "digraph G {\n";
        (fg:black bg:red)-|(fg:red)    fprintf out "  rankdir = BT\n";
        (fg:black bg:red)-|(fg:red)    let handle_descendant (type a) (from : a Node.(off)t(fg:red)) =
        (fg:black bg:red)-|(fg:red)      let from_name = node_name from in
        (fg:black bg:red)-|(fg:red)      fprintf out "  %s [label="%s %s\nheight = %d"]\n"
        (fg:black bg:red)-|(fg:red)        from_name from_name (Kind.name from.kind) from.height;
        (fg:black bg:red)-|(fg:red)      Node.iteri_parents from ~f:(fun _ to_ ->
        (fg:black bg:red)-|(fg:red)        fprintf out "  %s -> %s\n" from_name (node_name to_));
        (fg:black bg:red)-|(fg:red)      begin match from.kind with
        (fg:black bg:red)-|(fg:red)      | Bind_lhs_change bind ->
        (fg:black bg:red)-|(fg:red)        Bind.iter_nodes_created_on_rhs bind ~f:(fun to_ ->
        (fg:black bg:red)-|(fg:red)          fprintf out "  %s -> %s [style=dashed]\n" from_name (node_name to_(off))(fg:red));
        (fg:black bg:red)-|(fg:red)      | _ -> ()
        (fg:black bg:red)-|(fg:red)      end
        (fg:black bg:red)-|(fg:red)    in
        (fg:black bg:red)-|(off)    (fg:red)iter_observer_descendants t (off)~f(fg:red):handle_descendant;
        (fg:black bg:red)-|(fg:red)    fprintf out "}\n%!")
        (fg:black bg:green)+|(off)  Node.Packed.iter_descendants ((fg:green)directly_observed(off) t) ~f
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let amodule = "module App = struct
  let main = main
  let appname = \"carole-exploder\"
  let default_appdir = Some \"/j/office/app/carole/exploder\"
  let before_async = ignore
  let instances_in_subdir = false
  let multi_machine = false
end
"

let bmodule = "let compile_command =
  Command.async_basic
    ~summary:\"compile a Carole config repo\"
    Command.Spec.(
      empty
      +> anon (\"repo\" %: file)
    )
    (fun dir () ->
       read_file_list ~dir >>= fun files ->
       let files =
         List.filter_map files ~f:(fun file ->
           if is_ml_file_name file then Some (dir ^/ file) else None)
       in
       Carole_plugin.Intf.load files >>= function
       | Error error ->
         Printf.printf \"Can't compile: %s\\n%!\" error;
         exit 1
       | Ok all_systems ->
         Printf.printf \"Compiles: \\n%s\\n%!\"
           (All_systems_pretty_sexp.create all_systems
            |> All_systems_pretty_sexp.sexp_of_t
            |> Sexp.to_string_hum
           );
         exit 0
    )
"


let%expect_test "Delete spurious matches, part 2 - whitespace ought not to match" =
  let%bind () = patdiff ~mine:amodule ~other:bmodule [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,8 +1,25(off) ============================================================
        (fg:black bg:red)-|(fg:red)module(off) (fg:red)App(off) = (fg:red)struct
        (fg:black bg:red)-|(off)  (fg:red)let(off) (fg:red)main(off) (fg:red)=(off) (fg:red)main
        (fg:black bg:red)-|(off)  (fg:red)let(off) (fg:red)appname(off) (fg:red)=(off) "(fg:red)carole-exploder(off)"
        (fg:black bg:red)-|(off)  let (fg:red)default_appdir(off) = Some "(fg:red)/j/office/app/carole/exploder(off)"
        (fg:black bg:red)-|(off)  (fg:red)let(off) (fg:red)before_async(off) (fg:red)=(off) (fg:red)ignore
        (fg:black bg:red)-|(off)  (fg:red)let(off) (fg:red)instances_in_subdir(off) (fg:red)=(off) (fg:red)false
        (fg:black bg:red)-|(off)  (fg:red)let(off) (fg:red)multi_machine(off) (fg:red)=(off) (fg:red)false
        (fg:black bg:red)-|(fg:red)end
        (fg:black bg:green)+|(fg:green)let(off) (fg:green)compile_command(off) =
        (fg:black bg:green)+|(off)  (fg:green)Command.async_basic
        (fg:black bg:green)+|(off)    (fg:green)~summary:"compile(off) (fg:green)a(off) (fg:green)Carole(off) (fg:green)config(off) (fg:green)repo"
        (fg:black bg:green)+|(off)    (fg:green)Command.Spec.(
        (fg:black bg:green)+|(off)      (fg:green)empty
        (fg:black bg:green)+|(fg:green)      +> anon(off) (fg:green)((off)"(fg:green)repo(off)"(fg:green) %: file)
        (fg:black bg:green)+|(fg:green)    )
        (fg:black bg:green)+|(fg:green)    (fun dir () ->
        (fg:black bg:green)+|(off)       (fg:green)read_file_list ~dir >>= fun files ->
        (fg:black bg:green)+|(fg:green)       (off)let (fg:green)files(off) =
        (fg:black bg:green)+|(fg:green)         List.filter_map files ~f:(fun file ->
        (fg:black bg:green)+|(fg:green)           if is_ml_file_name file then(off) Some (fg:green)(dir ^/ file) else None)
        (fg:black bg:green)+|(fg:green)       in
        (fg:black bg:green)+|(fg:green)       Carole_plugin.Intf.load files >>= function
        (fg:black bg:green)+|(fg:green)       | Error error ->
        (fg:black bg:green)+|(fg:green)         Printf.printf (off)"(fg:green)Can't compile: %s\n%!(off)" (fg:green)error;
        (fg:black bg:green)+|(off)         (fg:green)exit(off) (fg:green)1
        (fg:black bg:green)+|(off)       (fg:green)|(off) (fg:green)Ok(off) (fg:green)all_systems(off) (fg:green)->
        (fg:black bg:green)+|(off)         (fg:green)Printf.printf(off) (fg:green)"Compiles:(off) (fg:green)\n%s\n%!"
        (fg:black bg:green)+|(off)           (fg:green)(All_systems_pretty_sexp.create all_systems
        (fg:black bg:green)+|(fg:green)            |> All_systems_pretty_sexp.sexp_of_t
        (fg:black bg:green)+|(fg:green)            |> Sexp.to_string_hum
        (fg:black bg:green)+|(fg:green)           );
        (fg:black bg:green)+|(fg:green)         exit(off) (fg:green)0
        (fg:black bg:green)+|(off)    (fg:green))
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let ascattered = "  let pred t select =
    with_return (fun {return} ->
      let select token = match select token with Some x -> x | None -> return None in
      let eval_term term =
        Eval_float.eval term ~f:(Bignum_token_or_constant.eval ~select)
      in
      Olang.eval t ~compare:(fun t1 t2 ->
        Bignum_float.compare (eval_term t1) (eval_term t2))
      |> Option.some)
"

let bscattered = "  let eval_filter (field, num_sel) oinfo =
    select_opt oinfo ~eval:(Numeric_selector.eval num_sel)
      ~sel:(fun oinfo ->
        Oinfo.From_server.float_field oinfo field)
"

let%expect_test "Delete spurious matches, part 3 - shorter example with a lot of chaff" =
  let%bind () = patdiff ~mine:ascattered ~other:bscattered [] in
  [%expect {|
    (fg:red)------ (+bold)mine
    (fg:green)++++++ (+bold)other
    (fg:black)@|(+bold)-1,9 +1,4(off) ============================================================
    (fg:black bg:red)-|(off)  let (fg:red)pred(off) (fg:red)t select =
    (fg:black bg:red)-|(fg:red)    with_return (off)((fg:red)fun {return} ->
    (fg:black bg:red)-|(fg:red)      let select token = match select token with Some x -> x | None(off) (fg:red)-> return None in
    (fg:black bg:red)-|(fg:red)      let eval_term(off) (fg:red)term(off) =
    (fg:black bg:red)-|(off)        (fg:red)Eval_float.eval(off) (fg:red)term(off) (fg:red)~f(off):((fg:red)Bignum_token_or_constant(off).eval (fg:red)~select(off))
    (fg:black bg:red)-|(fg:red)      in
    (fg:black bg:red)-|(fg:red)      Olang.eval(off) (fg:red)t ~compare(off):(fun(fg:red) t1(off) (fg:red)t2(off) ->
    (fg:black bg:red)-|(off)        (fg:red)Bignum_float(off).(fg:red)compare (eval_term t1) (eval_term t2))
    (fg:black bg:red)-|(off)      (fg:red)|>(off) (fg:red)Option.some(off))
    (fg:black bg:green)+|(off)  let (fg:green)eval_filter(off) ((fg:green)field,(off) (fg:green)num_sel)(off) (fg:green)oinfo(off) =
    (fg:black bg:green)+|(off)    (fg:green)select_opt(off) (fg:green)oinfo(off) (fg:green)~eval(off):((fg:green)Numeric_selector(off).eval (fg:green)num_sel(off))
    (fg:black bg:green)+|(off)      (fg:green)~sel(off):(fun (fg:green)oinfo(off) ->
    (fg:black bg:green)+|(off)        (fg:green)Oinfo(off).(fg:green)From_server.float_field(off) (fg:green)oinfo(off) (fg:green)field(off))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
