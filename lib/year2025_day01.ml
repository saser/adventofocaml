open Base
open Stdio

module Instruction = struct
  type t =
    { direction : [ `L | `R ]
    ; amount : int
    }
  [@@deriving sexp]

  let parse line =
    { direction =
        (match line.[0] with
         | 'L' -> `L
         | 'R' -> `R
         | _ -> failwith ("invalid line: " ^ line))
    ; amount = Int.of_string (String.drop_prefix line 1)
    }
  ;;

  let apply t dial =
    let turn amount dial =
      let zeroes = amount / 100 in
      let dial' = (dial + amount) % 100 in
      if dial' < dial then dial', zeroes + 1 else dial', zeroes
    in
    match t.direction with
    | `L ->
      (* The 100-x stuff here is to "reverse" the turning of the dial, from
      turning left to turning right. A bit like if you were to look at the dial
      in a mirror. Starting at 25 and turning L40 would leave you at 85;
      starting at 100-25=75 and turning R40 would leave you at 15, which gives
      100-15=85. *)
      let new_dial, zeroes = turn t.amount ((100 - dial) % 100) in
      (100 - new_dial) % 100, zeroes
    | `R -> turn t.amount dial
  ;;

  let apply_all ts dial =
    let rec loop ts dial acc_dials acc_zeroes =
      match ts with
      | [] -> acc_dials, acc_zeroes
      | t :: tl ->
        let dial', zeroes = apply t dial in
        loop tl dial' (dial' :: acc_dials) (zeroes + acc_zeroes)
    in
    loop ts dial [ dial ] 0
  ;;
end

let parse input = String.split_lines input |> List.map ~f:Instruction.parse

let solve input part =
  let instructions = parse input in
  let dials, zeroes = Instruction.apply_all instructions 50 in
  match part with
  | `Part1 -> List.count dials ~f:(( = ) 0)
  | `Part2 -> zeroes
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

let%expect_test "Instruction.apply" =
  let test dial s =
    let instruction = Instruction.parse s in
    let dial', zeroes = Instruction.apply instruction dial in
    printf "%d + %s = %d (zeroes: %d)\n" dial s dial' zeroes
  in
  List.iter
    ~f:(fun (dial, s) -> test dial s)
    [ 50, "R86"; 75, "R25"; 75, "L75"; 10, "R110"; 10, "L110"; 50, "L1000"; 50, "R1000" ];
  [%expect
    {|
    50 + R86 = 36 (zeroes: 1)
    75 + R25 = 0 (zeroes: 1)
    75 + L75 = 0 (zeroes: 1)
    10 + R110 = 20 (zeroes: 1)
    10 + L110 = 0 (zeroes: 2)
    50 + L1000 = 50 (zeroes: 10)
    50 + R1000 = 50 (zeroes: 10)
    |}];
  let dial = ref 50 in
  String.split_lines example_input
  |> List.iter ~f:(fun line ->
    let dial', zeroes = Instruction.apply (Instruction.parse line) !dial in
    printf "%d + %s = %d (zeroes: %d)\n" !dial line dial' zeroes;
    dial := dial');
  [%expect
    {|
    50 + L68 = 82 (zeroes: 1)
    82 + L30 = 52 (zeroes: 0)
    52 + R48 = 0 (zeroes: 1)
    0 + L5 = 95 (zeroes: 0)
    95 + R60 = 55 (zeroes: 1)
    55 + L55 = 0 (zeroes: 1)
    0 + L1 = 99 (zeroes: 0)
    99 + L99 = 0 (zeroes: 1)
    0 + R14 = 14 (zeroes: 0)
    14 + L82 = 32 (zeroes: 1)
    |}]
;;

let%expect_test "Instruction.apply_all" =
  let test input =
    let instructions = parse input in
    List.iter instructions ~f:(fun instr -> print_s [%sexp (instr : Instruction.t)]);
    let dials, zeroes = Instruction.apply_all instructions 50 in
    print_endline "dials:";
    print_s [%sexp (dials : int list)];
    printf "zeroes: %d\n" zeroes
  in
  test example_input;
  [%expect
    {|
    ((direction L) (amount 68))
    ((direction L) (amount 30))
    ((direction R) (amount 48))
    ((direction L) (amount 5))
    ((direction R) (amount 60))
    ((direction L) (amount 55))
    ((direction L) (amount 1))
    ((direction L) (amount 99))
    ((direction R) (amount 14))
    ((direction L) (amount 82))
    dials:
    (32 14 0 99 0 55 95 0 52 82 50)
    zeroes: 6
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
