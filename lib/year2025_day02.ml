open Base
open Stdio

(** [parse_range s] parses [s] as a string of the form ["x-y"] and returns [x,
y]. *)
let parse_range s =
  match String.split s ~on:'-' with
  | [ min; max ] -> Int.of_string min, Int.of_string max
  | _ -> Printf.failwithf "invalid line: %S" s ()
;;

let is_repeated_exactly_twice n =
  let s = Int.to_string n in
  let mid = String.length s / 2 in
  String.(subo s ~len:mid = subo s ~pos:mid)
;;

let is_repeated_at_least_twice n =
  (* This wonderful function comes from this StackOverflow question:
     https://stackoverflow.com/questions/55823298/how-do-i-check-if-a-string-is-entirely-made-of-the-same-substring. *)
  let s = Int.to_string n in
  let ss = s ^ s in
  String.is_substring (String.sub ss ~pos:1 ~len:(String.length ss - 2)) ~substring:s
;;

let solve input part =
  String.split input ~on:','
  |> List.map ~f:parse_range
  |> List.concat_map ~f:(fun (min, max) ->
    List.range ~start:`inclusive ~stop:`inclusive min max
    |> List.filter
         ~f:
           (match part with
            | `Part1 -> is_repeated_exactly_twice
            | `Part2 -> is_repeated_at_least_twice))
  |> List.sum (module Int) ~f:Fn.id
;;

let part1 input = solve input `Part1
let part2 input = solve input `Part2

let%expect_test "is_repeated" =
  let examples =
    [ 11
    ; 111
    ; 1112
    ; 11121112
    ; 121212
    ; 12121212
    ; 123123
    ; 123123123
    ; 123123123123
    ; 127895
    ; 123123124
    ]
  in
  Ascii_table.simple_list_table
    [ "n"; "is_repeated_exactly_twice n"; "is_repeated_at_least_twice n" ]
    (List.map examples ~f:(fun n ->
       [ Int.to_string n
       ; Bool.to_string (is_repeated_exactly_twice n)
       ; Bool.to_string (is_repeated_at_least_twice n)
       ]));
  [%expect
    {|
    ┌──────────────┬─────────────────────────────┬──────────────────────────────┐
    │            n │ is_repeated_exactly_twice n │ is_repeated_at_least_twice n │
    ├──────────────┼─────────────────────────────┼──────────────────────────────┤
    │           11 │                        true │                         true │
    │          111 │                       false │                         true │
    │         1112 │                       false │                        false │
    │     11121112 │                        true │                         true │
    │       121212 │                       false │                         true │
    │     12121212 │                        true │                         true │
    │       123123 │                        true │                         true │
    │    123123123 │                       false │                         true │
    │ 123123123123 │                        true │                         true │
    │       127895 │                       false │                        false │
    │    123123124 │                       false │                        false │
    └──────────────┴─────────────────────────────┴──────────────────────────────┘
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
