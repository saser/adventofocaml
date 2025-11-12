open Base
open Stdio

let parse input =
  String.filter ~f:(Char.( <> ) ':') input
  |> String.split_lines
  |> List.map ~f:(fun line -> String.split ~on:' ' line |> List.map ~f:Int.of_string)
;;

let ( ||| ) a b =
  let digits_in_b = 1 + Float.iround_down_exn (Float.log10 (Float.of_int b)) in
  (a * (10 ** digits_in_b)) + b
;;

let possible ?(allow_concat = false) numbers =
  let rec loop target acc numbers =
    if acc > target
    then false
    else (
      match numbers with
      | [] -> acc = target
      | x :: xs ->
        loop target (acc + x) xs
        || loop target (acc * x) xs
        || (allow_concat && loop target (acc ||| x) xs))
  in
  match numbers with
  | target :: x :: xs -> loop target x xs
  | _ -> failwith "malformed list of numbers"
;;

let part1 input =
  parse input |> List.filter ~f:possible |> List.sum (module Int) ~f:List.hd_exn
;;

let part2 input =
  parse input
  |> List.filter ~f:(possible ~allow_concat:true)
  |> List.sum (module Int) ~f:List.hd_exn
;;

let%expect_test "|||" =
  let test a b = printf "%d ||| %d = %d\n" a b (a ||| b) in
  test 12 345;
  test 10 10;
  test 10 90;
  [%expect
    {|
    12 ||| 345 = 12345
    10 ||| 10 = 1010
    10 ||| 90 = 1090
    |}]
;;

let%expect_test "possible" =
  let test numbers =
    printf
      "possible %s = %b\n"
      (Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t numbers))
      (possible numbers)
  in
  test [ 190; 10; 19 ];
  [%expect {| possible (190 10 19) = true |}];
  test [ 3267; 81; 40; 27 ];
  [%expect {| possible (3267 81 40 27) = true |}];
  test [ 83; 17; 5 ];
  [%expect {| possible (83 17 5) = false |}];
  test [ 156; 15; 6 ];
  [%expect {| possible (156 15 6) = false |}];
  test [ 7290; 6; 8; 6; 15 ];
  [%expect {| possible (7290 6 8 6 15) = false |}];
  test [ 161011; 16; 10; 13 ];
  [%expect {| possible (161011 16 10 13) = false |}];
  test [ 192; 17; 8; 14 ];
  [%expect {| possible (192 17 8 14) = false |}];
  test [ 21037; 9; 7; 18; 13 ];
  [%expect {| possible (21037 9 7 18 13) = false |}];
  test [ 292; 11; 6; 16; 20 ];
  [%expect {| possible (292 11 6 16 20) = true |}]
;;

let%expect_test "part1" =
  printf "part1: %d\n" (part1 Inputs.year2024_day07);
  printf "part2: %d\n" (part2 Inputs.year2024_day07);
  [%expect
    {|
    part1: 4998764814652
    part2: 37598910447546
    |}]
;;
