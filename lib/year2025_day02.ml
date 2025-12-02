open Base
open Stdio

(** [count_digits x] returns the number of digits of [x] in base 10. *)
let count_digits x =
  Float.of_int x |> Float.log10 |> Float.round_down |> Int.of_float |> ( + ) 1
;;

(** [take_digits x ~n] returns an integer consisting of the first [n] digits of
[x]. Raises if [n > count_digits x]. *)
let take_digits x ~n = x / (10 ** (count_digits x - n))

(** [repeat_digits x ~n] returns the integer formed by repeating the digits of
[x] [n] times. *)
let repeat_digits x ~n =
  let d = 10 ** count_digits x in
  let acc = ref 0 in
  for _ = 0 to n - 1 do
    acc := (!acc * d) + x
  done;
  !acc
;;

(** [parse_range s] parses [s] as a string of the form ["x-y"] and returns [x,
y]. *)
let parse_range s =
  match String.split s ~on:'-' with
  | [ min; max ] -> Int.of_string min, Int.of_string max
  | _ -> Printf.failwithf "invalid line: %S" s ()
;;

(** [is_invalid x ~n] determines whether [x] consists of [n] repeated groups of digits. *)
let is_invalid x ~n =
  let dx = count_digits x in
  if dx % n <> 0 then false else repeat_digits (take_digits x ~n:(dx / n)) ~n = x
;;

(** [invalid_ids ~min ~max ~nmin ~nmax] returns the set of invalid IDs between
[min] and [max] (inclusive) formed by dividing into at least [nmin] and at most
[nmax] groups. The returned list may contain duplicates. *)
let invalid_ids ~min ~max ~nmin ~nmax =
  List.Cartesian_product.map2
    (List.range ~start:`inclusive ~stop:`inclusive min max)
    (List.range ~start:`inclusive ~stop:`inclusive nmin nmax)
    ~f:(fun x n -> Option.some_if (is_invalid x ~n) x)
  |> List.filter_map ~f:Fn.id
;;

let part1 input =
  String.split input ~on:','
  |> List.map ~f:parse_range
  |> List.concat_map ~f:(fun (min, max) -> invalid_ids ~min ~max ~nmin:2 ~nmax:2)
  |> List.sum (module Int) ~f:Fn.id
;;

let part2 input =
  String.split input ~on:','
  |> List.map ~f:parse_range
  |> List.concat_map ~f:(fun (min, max) ->
    invalid_ids ~min ~max ~nmin:2 ~nmax:(count_digits max))
  |> Hash_set.of_list (module Int)
  |> Hash_set.sum (module Int) ~f:Fn.id
;;

let%expect_test "n_digits" =
  let examples = [ 1; 2; 9; 10; 111; 99999; 100000; 18925; 37848 ] in
  Ascii_table.simple_list_table
    [ "x"; "n_digits x" ]
    (List.map examples ~f:(fun x -> [ x; count_digits x ] |> List.map ~f:Int.to_string));
  [%expect
    {|
    ┌────────┬────────────┐
    │      x │ n_digits x │
    ├────────┼────────────┤
    │      1 │          1 │
    │      2 │          1 │
    │      9 │          1 │
    │     10 │          2 │
    │    111 │          3 │
    │  99999 │          5 │
    │ 100000 │          6 │
    │  18925 │          5 │
    │  37848 │          5 │
    └────────┴────────────┘
    |}]
;;

let%expect_test "take_digits" =
  let examples = [ 10, 1; 10, 2; 123, 1; 123, 2; 123, 3 ] in
  Ascii_table.simple_list_table
    [ "x"; "n"; "take_digits x ~n" ]
    (List.map examples ~f:(fun (x, n) ->
       [ x; n; take_digits x ~n ] |> List.map ~f:Int.to_string));
  [%expect
    {|
    ┌─────┬───┬──────────────────┐
    │   x │ n │ take_digits x ~n │
    ├─────┼───┼──────────────────┤
    │  10 │ 1 │                1 │
    │  10 │ 2 │               10 │
    │ 123 │ 1 │                1 │
    │ 123 │ 2 │               12 │
    │ 123 │ 3 │              123 │
    └─────┴───┴──────────────────┘
    |}]
;;

let%expect_test "repeat_digits" =
  let examples = [ 10, 1; 10, 2; 123, 1; 123, 2; 123, 3 ] in
  Ascii_table.simple_list_table
    [ "x"; "n"; "repeat_digits x ~n" ]
    (List.map examples ~f:(fun (x, n) ->
       [ x; n; repeat_digits x ~n ] |> List.map ~f:Int.to_string));
  [%expect
    {|
    ┌─────┬───┬────────────────────┐
    │   x │ n │ repeat_digits x ~n │
    ├─────┼───┼────────────────────┤
    │  10 │ 1 │                 10 │
    │  10 │ 2 │               1010 │
    │ 123 │ 1 │                123 │
    │ 123 │ 2 │             123123 │
    │ 123 │ 3 │          123123123 │
    └─────┴───┴────────────────────┘
    |}]
;;

let%expect_test "is_invalid" =
  let examples = [ 11, 2; 12, 2; 13, 2; 22, 2; 95, 2; 96, 2; 99, 2; 100, 2; 115, 2 ] in
  Ascii_table.simple_list_table
    [ "x"; "n"; "is_invalid x ~n" ]
    (List.map examples ~f:(fun (x, n) ->
       [ Int.to_string x; Int.to_string n; Bool.to_string (is_invalid x ~n) ]));
  [%expect
    {|
    ┌─────┬───┬─────────────────┐
    │   x │ n │ is_invalid x ~n │
    ├─────┼───┼─────────────────┤
    │  11 │ 2 │            true │
    │  12 │ 2 │           false │
    │  13 │ 2 │           false │
    │  22 │ 2 │            true │
    │  95 │ 2 │           false │
    │  96 │ 2 │           false │
    │  99 │ 2 │            true │
    │ 100 │ 2 │           false │
    │ 115 │ 2 │           false │
    └─────┴───┴─────────────────┘
    |}]
;;

let%expect_test "invalid_ids" =
  let examples = [ 11, 22; 95, 115 ] in
  Ascii_table.simple_list_table
    [ "min"; "max"; "invalid_ids ~min ~max ~nmin:2 ~nmax:2" ]
    (List.map examples ~f:(fun (min, max) ->
       [ Int.to_string min
       ; Int.to_string max
       ; Sexp.to_string_hum
           (List.sexp_of_t Int.sexp_of_t (invalid_ids ~min ~max ~nmin:2 ~nmax:2))
       ]));
  [%expect
    {|
    ┌─────┬─────┬───────────────────────────────────────┐
    │ min │ max │ invalid_ids ~min ~max ~nmin:2 ~nmax:2 │
    ├─────┼─────┼───────────────────────────────────────┤
    │  11 │  22 │                               (11 22) │
    │  95 │ 115 │                                  (99) │
    └─────┴─────┴───────────────────────────────────────┘
    |}];
  (* A few samples from the example input for sanity checking. *)
  let examples =
    [ 998, 1012, 2, 3; 2121212118, 2121212124, 2, 10; 824824821, 824824827, 2, 9 ]
  in
  Ascii_table.simple_list_table
    [ "min"; "max"; "nmin"; "nmax"; "invalid_ids ~min ~max ~nmin ~nmax" ]
    (List.map examples ~f:(fun (min, max, nmin, nmax) ->
       [ Int.to_string min
       ; Int.to_string max
       ; Int.to_string nmin
       ; Int.to_string nmax
       ; Sexp.to_string_hum
           (List.sexp_of_t Int.sexp_of_t (invalid_ids ~min ~max ~nmin ~nmax))
       ]));
  [%expect
    {|
    ┌────────────┬────────────┬──────┬──────┬───────────────────────────────────┐
    │        min │        max │ nmin │ nmax │ invalid_ids ~min ~max ~nmin ~nmax │
    ├────────────┼────────────┼──────┼──────┼───────────────────────────────────┤
    │        998 │       1012 │    2 │    3 │                        (999 1010) │
    │ 2121212118 │ 2121212124 │    2 │   10 │                      (2121212121) │
    │  824824821 │  824824827 │    2 │    9 │                       (824824824) │
    └────────────┴────────────┴──────┴──────┴───────────────────────────────────┘
    |}]
;;

let example_input =
  String.strip
    {|
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
|}
;;

let%expect_test "solve" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect
    {|
    part1: 1227775554
    part2: 4174379265
    |}];
  test Inputs.year2025_day02;
  [%expect
    {|
    part1: 8576933996
    part2: 25663320831
    |}]
;;
