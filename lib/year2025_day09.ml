open Base
open Stdio

let example_input =
  String.strip
    {|
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
|}
;;

let part1 _input = -1
let part2 _input = -1

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect {|
    part1: -1
    part2: -1
    |}];
  test Inputs.year2025_day09;
  [%expect {|
    part1: -1
    part2: -1
    |}]
;;
