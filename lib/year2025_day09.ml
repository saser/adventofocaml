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

let part1 input =
  let reds =
    String.split_lines input
    |> List.map ~f:(fun line ->
      match String.split line ~on:',' with
      | [ x; y ] -> Int.of_string x, Int.of_string y
      | _ -> Printf.failwithf "invalid line: %S" line ())
    |> List.to_array
  in
  let x =
    Array.cartesian_product reds reds
    |> Array.map ~f:(fun ((x1, y1), (x2, y2)) ->
      (Int.abs (x1 - x2) + 1) * (Int.abs (y1 - y2) + 1))
    |> Array.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  x
;;

let part2 _input = -1

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect
    {|
    part1: 50
    part2: -1
    |}];
  test Inputs.year2025_day09;
  [%expect
    {|
    part1: 4763932976
    part2: -1
    |}]
;;
