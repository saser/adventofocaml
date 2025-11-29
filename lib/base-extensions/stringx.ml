open Base
open Stdio

let fields s =
  String.split_on_chars s ~on:[ '\t'; '\n'; '\r'; ' ' ]
  |> List.filter ~f:(Fn.non @@ String.is_empty)
;;

let%expect_test "fields" =
  let test s =
    printf
      "%S -> %s\n"
      s
      (fields s |> List.sexp_of_t String.sexp_of_t |> Sexp.to_string_hum)
  in
  let inputs =
    [ ""
    ; " "
    ; "a"
    ; "  a  "
    ; "a b c d"
    ; "       a  b       c d         "
    ; "\t\t\t\t\t\t\ta"
    ; {|
    a
    b
    c
    |}
    ]
  in
  List.iter inputs ~f:test;
  [%expect
    {|
    "" -> ()
    " " -> ()
    "a" -> (a)
    "  a  " -> (a)
    "a b c d" -> (a b c d)
    "       a  b       c d         " -> (a b c d)
    "\t\t\t\t\t\t\ta" -> (a)
    "\n    a\n    b\n    c\n    " -> (a b c)
    |}]
;;
