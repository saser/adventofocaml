(* This file is just for playing around with the language, preprocessors, libraries, etc. *)

open Base
open Stdio

(* Example of using '@@deriving sexp' for a custom type. *)
type foo =
  | Bar of string
  | Baz of int
  | Quux of string * string
[@@deriving sexp]

let%expect_test "sexp_of_foo" =
  let test_single f = f |> sexp_of_foo |> Sexp.to_string_hum |> print_endline in
  test_single (Bar "asdf");
  [%expect {| (Bar asdf) |}];
  test_single (Quux ("abc", "def"));
  [%expect {| (Quux abc def) |}]
;;

(* Same but for a recursive/self-referencing type. *)
type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree
[@@deriving sexp]

let%expect_test "sexp_of_tree" =
  let l s = Leaf s in
  let n t1 t2 = Node (t1, t2) in
  let test (t : string tree) =
    t |> sexp_of_tree String.sexp_of_t |> Sexp.to_string_hum |> print_endline
  in
  test (l "123");
  [%expect {| (Leaf 123) |}];
  test (n (l "abc") (l "123"));
  [%expect {| (Node (Leaf abc) (Leaf 123)) |}];
  test
    (n
       (n (n (l "1") (l "2")) (n (l "3") (l "4")))
       (n (n (l "5") (l "6")) (n (l "7") (l "8"))));
  [%expect
    {|
    (Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
     (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8))))
    |}]
;;
