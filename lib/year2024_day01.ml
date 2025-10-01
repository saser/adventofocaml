open Base
open Stdio

let parse input =
  let numbers =
    String.split_lines input
    |> List.concat_map ~f:(String.split ~on:' ')
    |> List.filter_map ~f:Int.of_string_opt
  in
  let rec loop (left, right) l =
    match l with
    | [] -> Ok (left, right)
    | a :: b :: tl -> loop (a :: left, b :: right) tl
    | _ ->
      Or_error.errorf
        "input contains %d numbers; expected an even number"
        (List.length numbers)
  in
  loop ([], []) numbers
;;

let part1 input =
  let open Or_error.Let_syntax in
  let%bind left, right = parse input in
  let left = List.sort ~compare:Int.compare left in
  let right = List.sort ~compare:Int.compare right in
  let rec loop left right sum =
    match left, right with
    | [], [] -> Ok sum
    | a :: tl_left, b :: tl_right -> loop tl_left tl_right (sum + Int.abs (a - b))
    | _ -> Or_error.error_string "unreachable"
  in
  loop left right 0
;;

let part2 input =
  let open Or_error.Let_syntax in
  let%bind left, right = parse input in
  let freq =
    List.fold right ~init:[] ~f:(fun alist n ->
      let k = Option.value (List.Assoc.find alist ~equal:Int.equal n) ~default:0 in
      List.Assoc.add alist ~equal:Int.equal n (k + 1))
  in
  let scores =
    List.map left ~f:(fun n ->
      let k = Option.value (List.Assoc.find freq ~equal:Int.equal n) ~default:0 in
      n * k)
  in
  let total_score = List.sum (module Int) scores ~f:Fn.id in
  Ok total_score
;;

let example_input =
  String.strip
    {|
3   4
4   3
2   5
1   3
3   9
3   3
|}
;;

let%expect_test "parse" =
  let test input =
    match parse input with
    | Ok (left, right) ->
      print_endline ("left:  " ^ Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t left));
      print_endline ("right: " ^ Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t right))
    | Error err -> print_endline ("error: " ^ Error.to_string_hum err)
  in
  test example_input;
  [%expect
    {|
    left:  (3 3 1 2 4 3)
    right: (3 9 3 5 3 4)
    |}]
;;

let%expect_test "part1" =
  let test input =
    match part1 input with
    | Ok sum -> printf "%d\n" sum
    | Error err -> print_endline ("error: " ^ Error.to_string_hum err)
  in
  test example_input;
  [%expect {| 11 |}];
  test Inputs.year2024_day01;
  [%expect {| 1579939 |}]
;;

let%expect_test "part2" =
  let test input =
    match part2 input with
    | Ok sum -> printf "%d\n" sum
    | Error err -> print_endline ("error: " ^ Error.to_string_hum err)
  in
  test example_input;
  [%expect {| 31 |}];
  test Inputs.year2024_day01;
  [%expect {| 20351745 |}]
;;
