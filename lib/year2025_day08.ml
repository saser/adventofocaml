open Base
open Stdio

let part1 _input : int = -1
let part2 _input : int = -1

let example_input =
  String.strip
    {|
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
|}
;;

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
  test Inputs.year2025_day08;
  [%expect {|
    part1: -1
    part2: -1
    |}]
;;
