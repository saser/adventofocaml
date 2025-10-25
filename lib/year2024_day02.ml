open! Base
open! Stdio

let parse_line line = String.split line ~on:' ' |> List.map ~f:Int.of_string
let parse input : int list list = String.split_lines input |> List.map ~f:parse_line

let rec is_safe_inc report : bool =
  match report with
  | [] -> true
  | [ _ ] -> true
  | a :: b :: xs -> Int.between ~low:1 ~high:3 (b - a) && is_safe_inc (b :: xs)
;;

let is_safe_dec report : bool = is_safe_inc (List.rev report)
let is_safe report : bool = is_safe_inc report || is_safe_dec report
let is_safe_step a b = Int.between ~low:1 ~high:3 (b - a)

let rec is_skip_safe_inc report : bool =
  match report with
  | [] | [ _ ] | [ _; _ ] -> true
  | x :: y :: z :: rest ->
    if is_safe_step x y && is_safe_step y z
    then is_skip_safe_inc (y :: z :: rest)
    else
      false
      (* skip x *)
      || is_safe_inc (y :: z :: rest)
      (* skip y *)
      || is_safe_inc (x :: z :: rest)
      (* skip z *)
      || is_safe_inc (x :: y :: rest)
;;

let is_skip_safe_dec report : bool = is_skip_safe_inc (List.rev report)
let is_skip_safe report : bool = is_skip_safe_inc report || is_skip_safe_dec report
let solve input ~f = parse input |> List.count ~f
let part1 = solve ~f:is_safe
let part2 = solve ~f:is_skip_safe

let%expect_test "solution" =
  printf "part 1: %d\n" (part1 Inputs.year2024_day02);
  printf "part 2: %d\n" (part2 Inputs.year2024_day02);
  [%expect
    {|
    part 1: 246
    part 2: 318
    |}]
;;

let%expect_test "parse_line" =
  let test line =
    printf "%S -> %s\n" line (Sexp.to_string_hum [%sexp (parse_line line : int list)])
  in
  let examples =
    [ "7 6 4 2 1"; "1 2 7 8 9"; "9 7 6 2 1"; "1 3 2 4 5"; "8 6 4 4 1"; "1 3 6 7 9" ]
  in
  List.iter examples ~f:test;
  [%expect
    {|
    "7 6 4 2 1" -> (7 6 4 2 1)
    "1 2 7 8 9" -> (1 2 7 8 9)
    "9 7 6 2 1" -> (9 7 6 2 1)
    "1 3 2 4 5" -> (1 3 2 4 5)
    "8 6 4 4 1" -> (8 6 4 4 1)
    "1 3 6 7 9" -> (1 3 6 7 9)
    |}]
;;

let%expect_test "parse" =
  let test input =
    parse input |> List.iter ~f:(fun report -> print_s [%sexp (report : int list)])
  in
  test
    (String.strip
       {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
  |});
  [%expect
    {|
    (7 6 4 2 1)
    (1 2 7 8 9)
    (9 7 6 2 1)
    (1 3 2 4 5)
    (8 6 4 4 1)
    (1 3 6 7 9)
    |}]
;;

let%expect_test "is_safe" =
  let row line =
    let report = parse_line line in
    line
    :: List.map
         ~f:Bool.to_string
         [ is_safe_inc report; is_safe_dec report; is_safe report ]
  in
  Ascii_table.simple_list_table
    [ "input"; "is_safe_inc"; "is_safe_dec"; "is_safe" ]
    (List.map
       ~f:row
       [ "7 6 4 2 1"
       ; "1 2 7 8 9"
       ; "9 7 6 2 1"
       ; "1 3 2 4 5"
       ; "8 6 4 4 1"
       ; "1 3 6 7 9"
       ; "1 2 3 4 5 6 7"
       ]);
  [%expect
    {|
    ┌───────────────┬─────────────┬─────────────┬─────────┐
    │         input │ is_safe_inc │ is_safe_dec │ is_safe │
    ├───────────────┼─────────────┼─────────────┼─────────┤
    │     7 6 4 2 1 │       false │        true │    true │
    │     1 2 7 8 9 │       false │       false │   false │
    │     9 7 6 2 1 │       false │       false │   false │
    │     1 3 2 4 5 │       false │       false │   false │
    │     8 6 4 4 1 │       false │       false │   false │
    │     1 3 6 7 9 │        true │       false │    true │
    │ 1 2 3 4 5 6 7 │        true │       false │    true │
    └───────────────┴─────────────┴─────────────┴─────────┘
    |}]
;;
