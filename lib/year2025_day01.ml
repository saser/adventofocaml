open Base
open Stdio

let parse_line line =
  let sign =
    match line.[0] with
    | 'L' -> -1
    | 'R' -> 1
    | _ -> failwith (Printf.sprintf "invalid line: %S" line)
  in
  sign * Int.of_string (String.drop_prefix line 1)
;;

let apply dial instruction =
  let turn_right dial amount =
    let zeroes = amount / 100 in
    let dial' = (dial + amount) % 100 in
    if dial' < dial then dial', zeroes + 1 else dial', zeroes
  in
  if instruction < 0
  then (
    (* The 100-x stuff here is to "mirror" the turning of the dial, from turning
    left to turning right. For example: starting at 25 and turning L40 would
    leave you at 85 crossing zero once; starting at 100-25=75 and turning R40
    would leave you at 100-85=15 crossing zero once. *)
    let dial', zeroes = turn_right ((100 - dial) % 100) (Int.abs instruction) in
    (100 - dial') % 100, zeroes)
  else turn_right dial instruction
;;

let solve input part =
  let _, exact_zeroes, total_zeroes =
    String.split_lines input
    |> List.map ~f:parse_line
    |> List.fold
         ~init:(50, 0, 0)
         ~f:(fun (dial, exact_zeroes, total_zeroes) instruction ->
           let dial', new_zeroes = apply dial instruction in
           dial', exact_zeroes + Bool.to_int (dial' = 0), total_zeroes + new_zeroes)
  in
  match part with
  | `Part1 -> exact_zeroes
  | `Part2 -> total_zeroes
;;

let part1 input = solve input `Part1
let part2 input = solve input `Part2

let example_input =
  String.strip
    {|
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
|}
;;

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect
    {|
    part1: 3
    part2: 6
    |}];
  test Inputs.year2025_day01;
  [%expect
    {|
    part1: 969
    part2: 5887
    |}]
;;
