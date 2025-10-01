open! Core
open! Expect_test_helpers_base
open Expect_test_patdiff

let strings = "zero one two three" |> String.split ~on:' '
let sorted_strings = List.sort strings ~compare:String.compare
let string1 = strings |> String.concat ~sep:"\n"
let string2 = sorted_strings |> String.concat ~sep:"\n"

let%expect_test "print_patdiff" =
  print_patdiff string1 string1;
  [%expect {| |}];
  print_patdiff string2 string2;
  [%expect {| |}];
  print_patdiff string1 string2;
  [%expect
    {|
    === DIFF HUNK ===
    -|zero
      one
    +|three
      two
    -|three
    +|zero
    |}];
  print_patdiff string2 string1 ~context:0;
  [%expect
    {|
    === DIFF HUNK ===
    +|zero
    === DIFF HUNK ===
    +|two
    === DIFF HUNK ===
    -|two
    -|zero
    |}]
;;

let sexp1 =
  [%sexp
    Node
      { left = Leaf
      ; key = "one"
      ; right =
          Node
            { left = Leaf
            ; key = "two"
            ; right = Node { left = Leaf; key = "three"; right = Leaf }
            }
      }]
;;

let sexp2 =
  [%sexp
    Node
      { left =
          Node
            { left = Node { left = Leaf; key = "one"; right = Leaf }
            ; key = "two"
            ; right = Leaf
            }
      ; key = "three"
      ; right = Leaf
      }]
;;

let%expect_test "print_patdiff_s" =
  print_s sexp1;
  [%expect
    {|
    (Node (
      (left Leaf)
      (key  one)
      (right (
        Node (
          (left Leaf)
          (key  two)
          (right (
            Node (
              (left  Leaf)
              (key   three)
              (right Leaf)))))))))
    |}];
  print_s sexp2;
  [%expect
    {|
    (Node (
      (left (
        Node (
          (left (
            Node (
              (left  Leaf)
              (key   one)
              (right Leaf))))
          (key   two)
          (right Leaf))))
      (key   three)
      (right Leaf)))
    |}];
  print_patdiff_s sexp1 sexp1;
  [%expect {| |}];
  print_patdiff_s sexp2 sexp2;
  [%expect {| |}];
  print_patdiff_s sexp1 sexp2;
  [%expect
    {|
    === DIFF HUNK ===
      (Node (
    -|  (left Leaf)
    -|  (key  one)
    -|  (right (
    +|  (left (
    +|    Node (
    +|      (left (
    -|    Node (
    -|      (left Leaf)
    -|      (key  two)
    +|        Node (
    +|          (left  Leaf)
    +|          (key   one)
    -|      (right (
    -|        Node (
    -|          (left  Leaf)
    +|          (right Leaf))))
    +|      (key   two)
    +|      (right Leaf))))
        (key   three)
    -|          (right Leaf)))))))))
    +|  (right Leaf)))
    |}];
  print_patdiff_s sexp2 sexp1 ~context:0;
  [%expect
    {|
    === DIFF HUNK ===
    -|  (left (
    -|    Node (
    -|      (left (
    +|  (left Leaf)
    +|  (key  one)
    +|  (right (
    -|        Node (
    -|          (left  Leaf)
    -|          (key   one)
    +|    Node (
    +|      (left Leaf)
    +|      (key  two)
    -|          (right Leaf))))
    -|      (key   two)
    -|      (right Leaf))))
    +|      (right (
    +|        Node (
    +|          (left  Leaf)
    === DIFF HUNK ===
    -|  (right Leaf)))
    +|          (right Leaf)))))))))
    |}]
;;

let%expect_test "non-empty patdiff prints a newline" =
  print_endline "----------";
  print_patdiff "cat" "dog";
  print_endline "----------";
  [%expect
    {|
    ----------
    === DIFF HUNK ===
    -|cat
    +|dog
    ----------
    |}]
;;

let%expect_test "empty patdiff does not print a newline" =
  print_endline "----------";
  print_patdiff "cat" "cat";
  print_endline "----------";
  [%expect
    {|
    ----------
    ----------
    |}]
;;
