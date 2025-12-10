open Base
open Stdio

(* Credit where credit is due: I did not come up with this solution myself. It's
   courtesy of Reddit user /u/tenthmascot who posted a beautiful explanation of
   it here: https://www.reddit.com/r/adventofcode/comments/1pk87hl.

   I had my own ideas for how to solve this, including doing some kind of linear
   algebra, but at the time I stumbled over this solution I lacked the energy to
   explore them further. *)

(** [Button] represents a button as an integer, where bit [b] being set means
    that pressing the button will flip light / increase joltage level [b]. *)
module Button = struct
  type t = int

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '(' || c = ')'))
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> List.fold ~init:0 ~f:(fun acc n -> Int.(acc lor (1 lsl n)))
  ;;

  let to_string t =
    List.filter_opt
      (List.init Int.num_bits ~f:(fun b -> Option.some_if Int.(t land (1 lsl b) <> 0) b))
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:","
    |> Printf.sprintf "(%s)"
  ;;

  let to_int t = t
end

(** [Lights] represents a set of lights as a bitmask, where bit [b] being set
    means that the light is on. *)
module Lights = struct
  type t = int [@@deriving hash, compare, sexp_of]

  let zero = 0

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '[' || c = ']'))
    |> String.foldi ~init:0 ~f:(fun i acc c ->
      match c with
      | '#' -> Int.(acc lor (1 lsl i))
      | '.' -> acc
      | _ -> failwith "unreachable")
  ;;

  let to_string t n =
    Array.init n ~f:(fun b -> if Int.(t land (1 lsl b) <> 0) then '#' else '.')
    |> String.of_array
    |> Printf.sprintf "[%s]"
  ;;

  let apply_all t buttons =
    Array.fold buttons ~init:t ~f:(fun acc b -> Int.(acc lxor Button.to_int b))
  ;;

  let equal = Int.( = )
  let of_int i = i
  let to_int t = t
end

(** [Joltage] represents the joltage levels as a list of integers. *)
module Joltage = struct
  type t = int list [@@deriving compare, hash, sexp_of]

  let zero n = List.init n ~f:(Fn.const 0)
  let is_zero t = List.for_all t ~f:(Int.( = ) 0)
  let is_valid t = List.for_all t ~f:Int.is_non_negative

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '{' || c = '}'))
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
  ;;

  let apply_all t buttons =
    let apply t button =
      let b = Button.to_int button in
      List.mapi t ~f:(fun i x -> if Int.(b land (1 lsl i)) <> 0 then x + 1 else x)
    in
    Array.fold buttons ~init:t ~f:apply
  ;;

  let length = List.length

  let parity t =
    List.foldi t ~init:0 ~f:(fun i acc n -> Int.(acc lor ((n % 2) lsl i)))
    |> Lights.of_int
  ;;

  let sub_and_halve t1 t2 = List.map2_exn t1 t2 ~f:(fun i j -> (i - j) / 2)
end

(** [bitsets ~n ~k] iterates over all [n]-bit integers with [k] bits set. *)
let bitsets ~n ~k =
  let rec generate acc ~n ~k =
    if k = 0
    then Sequence.return acc
    else if n < k
    then Sequence.empty
    else
      Sequence.append
        (generate Int.(acc lor (1 lsl (n - 1))) ~n:(n - 1) ~k:(k - 1))
        (generate acc ~n:(n - 1) ~k)
  in
  generate 0 ~n ~k
;;

(** [bitsets_ordered ~n] iterates over all [n]-bit integers with 0, 1, 2, ...,
    [n] bits set. *)
let bitsets_ordered ~n =
  Sequence.init (n + 1) ~f:(fun k -> bitsets ~n ~k) |> Sequence.concat
;;

(** [select arr bits] returns a new array containing only the elements in [arr]
    whose indices correspond to set bit indices in [bits]. *)
let select arr bits = Array.filteri arr ~f:(fun i _x -> Int.(bits land (1 lsl i)) <> 0)

(** [combinations arr] iterates over all possible combinations of elements in
    [arr], including the empty array. Combinations are returned in an undefined
    order. *)
let combinations arr =
  let n = Array.length arr in
  Sequence.range 0 Int.(1 lsl n) |> Sequence.map ~f:(select arr)
;;

(** [combinations_ordered] iterates over all possible combinations of elements
    in [arr]. Combinations are returned in order of their cardinality, i.e.
    containing 0, 1, 2, ..., [Array.length arr] elements. Within each
    cardinality the order of elements is undefined. *)
let combinations_ordered arr =
  let n = Array.length arr in
  bitsets_ordered ~n |> Sequence.map ~f:(select arr)
;;

(** [matching_combinations target buttons] iterates over the combinations of
    [buttons] that when pressed yields lights equal to [target] (starting from
    an initial state of all lights turned off). *)
let matching_combinations target buttons =
  Sequence.filter (combinations_ordered buttons) ~f:(fun comb ->
    Lights.equal (Lights.apply_all Lights.zero comb) target)
;;

let parse_single line =
  let fields = String.split line ~on:' ' |> List.to_array in
  let n = Array.length fields in
  let target = Lights.of_string fields.(0) in
  let buttons = Array.init (n - 2) ~f:(fun i -> Button.of_string fields.(i + 1)) in
  let joltage = Joltage.of_string (Array.last fields) in
  target, buttons, joltage
;;

let part1_single line =
  let target, buttons, _ = parse_single line in
  matching_combinations target buttons |> Sequence.hd_exn |> Array.length
;;

let part1 input = String.split_lines input |> List.sum (module Int) ~f:part1_single

(** [make_parity_cache buttons n] returns a lookup [cache] such that [cache.(p)]
    is the set of combinations of [buttons] that, when pressed, result in a
    parity of [p] (of [n] bits), represented as the accumulated joltage levels
    and number of buttons in the combination.

    For example, let's assume that [buttons] corresponds to [(2) (1, 3) (1, 2,
    3)] and that [n = 4]. If [let cache = make_parity_cache buttons n] then 
    [cache.(0b1010) = [[0;1;0;1], 1; [0;1;2;1], 2]] means that there are two
    ways to achieve a parity of [0b1010] (i.e. [[.#.#]]), one of which requires
    [1] button press and results in an accumulated joltage of [0;1;0;1]
    (pressing [(1, 3)]), and the other requires [2] button presses and results
    in an accumulated joltage of [0;1;2;1] (pressing [(2)] and [(1,2,3)]).
    *)
let make_parity_cache buttons n =
  let parity_count = Int.(1 lsl n) in
  let by_parity = Array.create ~len:parity_count [] in
  Sequence.iter (combinations buttons) ~f:(fun comb ->
    let accumulated = Joltage.apply_all (Joltage.zero n) comb in
    let parity = Lights.to_int (Joltage.parity accumulated) in
    by_parity.(parity) <- (accumulated, Array.length comb) :: by_parity.(parity));
  by_parity
;;

let part2_single line =
  let _, buttons, joltage = parse_single line in
  let n = Joltage.length joltage in
  let by_parity = make_parity_cache buttons n in
  let memo = Hashtbl.create (module Joltage) in
  let rec min_presses joltage =
    match Hashtbl.find memo joltage with
    | Some result -> result
    | None ->
      let result =
        if Joltage.is_zero joltage
        then Some 0
        else
          by_parity.(Joltage.parity joltage |> Lights.to_int)
          |> List.filter_map ~f:(fun (accumulated, accumulated_cost) ->
            let next = Joltage.sub_and_halve joltage accumulated in
            if Joltage.is_valid next
            then
              min_presses next |> Option.map ~f:(fun sub -> (2 * sub) + accumulated_cost)
            else None)
          |> List.min_elt ~compare:Int.compare
      in
      Hashtbl.set memo ~key:joltage ~data:result;
      result
  in
  min_presses joltage
;;

let part2 input =
  String.split_lines input
  |> List.sum
       (module Int)
       ~f:(fun line -> part2_single line |> Option.value ~default:(-1000000))
;;

let example_input =
  String.strip
    {|
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
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
    part1: 7
    part2: 33
    |}];
  test Inputs.year2025_day10;
  [%expect
    {|
    part1: 505
    part2: 20002
    |}]
;;

let%expect_test "combinations" =
  let test arr =
    Sequence.iter (combinations_ordered arr) ~f:(fun comb ->
      print_endline (Sexp.to_string_hum [%sexp (comb : int array)]))
  in
  test [||];
  [%expect {| () |}];
  test [| 1 |];
  [%expect
    {|
    ()
    (1)
    |}];
  test [| 1; 2 |];
  [%expect
    {|
    ()
    (2)
    (1)
    (1 2)
    |}];
  test [| 1; 2; 3; 4 |];
  [%expect
    {|
    ()
    (4)
    (3)
    (2)
    (1)
    (3 4)
    (2 4)
    (1 4)
    (2 3)
    (1 3)
    (1 2)
    (2 3 4)
    (1 3 4)
    (1 2 4)
    (1 2 3)
    (1 2 3 4)
    |}]
;;

let%expect_test "bitsets" =
  let test ~n ~k =
    Sequence.iter (bitsets ~n ~k) ~f:(fun bits ->
      print_endline (Int.Binary.to_string_hum bits))
  in
  test ~n:5 ~k:0;
  [%expect {| 0b0 |}];
  test ~n:5 ~k:3;
  [%expect
    {|
    0b1_1100
    0b1_1010
    0b1_1001
    0b1_0110
    0b1_0101
    0b1_0011
    0b1110
    0b1101
    0b1011
    0b111
    |}];
  test ~n:5 ~k:5;
  [%expect {| 0b1_1111 |}]
;;

let%expect_test "bitsets_ordered" =
  let test ~n =
    Sequence.iter (bitsets_ordered ~n) ~f:(fun bits ->
      print_endline (Int.Binary.to_string_hum bits))
  in
  test ~n:3;
  [%expect
    {|
    0b0
    0b100
    0b10
    0b1
    0b110
    0b101
    0b11
    0b111
    |}];
  test ~n:5;
  [%expect
    {|
    0b0
    0b1_0000
    0b1000
    0b100
    0b10
    0b1
    0b1_1000
    0b1_0100
    0b1_0010
    0b1_0001
    0b1100
    0b1010
    0b1001
    0b110
    0b101
    0b11
    0b1_1100
    0b1_1010
    0b1_1001
    0b1_0110
    0b1_0101
    0b1_0011
    0b1110
    0b1101
    0b1011
    0b111
    0b1_1110
    0b1_1101
    0b1_1011
    0b1_0111
    0b1111
    0b1_1111
    |}]
;;

let%expect_test "matching_combinations" =
  let test target buttons =
    let target = Lights.of_string target in
    let buttons =
      String.split buttons ~on:' ' |> List.map ~f:Button.of_string |> List.to_array
    in
    Sequence.iter (matching_combinations target buttons) ~f:(fun comb ->
      Array.map comb ~f:Button.to_string
      |> List.of_array
      |> String.concat ~sep:" "
      |> print_endline)
  in
  test "[.##.]" "(3) (1,3) (2) (2,3) (0,2) (0,1)";
  [%expect
    {|
    (0,2) (0,1)
    (1,3) (2,3)
    (3) (1,3) (2)
    (3) (2) (2,3) (0,2) (0,1)
    |}];
  test "[...#.]" "(0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4)";
  [%expect
    {|
    (0,4) (0,1,2) (1,2,3,4)
    (0,2,3,4) (2,3) (0,1,2) (1,2,3,4)
    |}]
;;
