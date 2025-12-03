open Base
open Base_extensions
open Stdio

let parse line =
  String.to_array line |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0')
;;

let max_joltage digits ~k =
  let digits = ref digits in
  let joltage = ref 0 in
  (* Greedy solution: pick the *first* maximum digit that leaves enough room
  for the rest of the number, and repeat that until all digits have been found.
  This works because it's always better to pick a higher digit than a lower one:
  e.g. any k-digit number that begins with a 8 is always going to be better than
  any k-digit number that begins with 7, 6, etc. By leaving at least (k-1)
  digits at the end, we ensure we can create a k-digit number.
  
  It's important to pick the first maximum digit in each iteration so that there
  are as many candidates as possible for the next iteration. E.g. if digits =
  [9, 3, 9, 1] and we want to create a 2-digit number, we need to pick the first
  9 because it means we can pick a second 9. *)
  for k' = k downto 1 do
    let digits' = Array.subo !digits ~len:(Array.length !digits - k' + 1) in
    let i = Arrayx.max_idx digits' ~compare:Int.compare |> Option.value_exn in
    joltage := (!joltage * 10) + digits'.(i);
    digits := Array.subo !digits ~pos:(i + 1)
  done;
  !joltage
;;

let solve input part =
  let k =
    match part with
    | `Part1 -> 2
    | `Part2 -> 12
  in
  String.split_lines input
  |> List.map ~f:parse
  |> List.sum (module Int) ~f:(max_joltage ~k)
;;

let part1 input = solve input `Part1
let part2 input = solve input `Part2

let example_input =
  String.strip
    {|
987654321111111
811111111111119
234234234234278
818181911112111
|}
;;

let%expect_test "parse" =
  let test input =
    Ascii_table.simple_list_table
      [ "line"; "parse line" ]
      (List.map (String.split_lines input) ~f:(fun line ->
         [ line; Sexp.to_string_hum [%sexp (parse line : int array)] ]))
  in
  test example_input;
  [%expect
    {|
    ┌─────────────────┬─────────────────────────────────┐
    │            line │                      parse line │
    ├─────────────────┼─────────────────────────────────┤
    │ 987654321111111 │ (9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) │
    │ 811111111111119 │ (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9) │
    │ 234234234234278 │ (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) │
    │ 818181911112111 │ (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1) │
    └─────────────────┴─────────────────────────────────┘
    |}]
;;

let%expect_test "max_joltage" =
  let test input =
    Ascii_table.simple_list_table
      [ "line"; "digits"; "max_joltage ~k:2"; "max_joltage ~k:12" ]
      (List.map (String.split_lines input) ~f:(fun line ->
         let digits = parse line in
         [ line
         ; Sexp.to_string_hum [%sexp (digits : int array)]
         ; Int.to_string (max_joltage digits ~k:2)
         ; Int.to_string (max_joltage digits ~k:12)
         ]))
  in
  test example_input;
  [%expect
    {|
    ┌─────────────────┬─────────────────────────────────┬──────────────────┬───────────────────┐
    │            line │                          digits │ max_joltage ~k:2 │ max_joltage ~k:12 │
    ├─────────────────┼─────────────────────────────────┼──────────────────┼───────────────────┤
    │ 987654321111111 │ (9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) │               98 │      987654321111 │
    │ 811111111111119 │ (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9) │               89 │      811111111119 │
    │ 234234234234278 │ (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) │               78 │      434234234278 │
    │ 818181911112111 │ (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1) │               92 │      888911112111 │
    └─────────────────┴─────────────────────────────────┴──────────────────┴───────────────────┘
    |}]
;;

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect
    {|
    part1: 357
    part2: 3121910778619
    |}];
  test Inputs.year2025_day03;
  [%expect
    {|
    part1: 17766
    part2: 176582889354075
    |}]
;;
