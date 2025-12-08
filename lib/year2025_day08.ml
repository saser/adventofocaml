open Base
open Stdio

module Pos3 = struct
  type t =
    { x : float
    ; y : float
    ; z : float
    }
  [@@deriving compare, hash, sexp_of]

  let of_string s =
    match String.split s ~on:',' with
    | [ x; y; z ] ->
      { x = Float.of_string x; y = Float.of_string y; z = Float.of_string z }
    | _ -> Printf.failwithf "invalid line %S" s ()
  ;;

  let to_string t =
    Float.(Printf.sprintf "%d,%d,%d" (to_int t.x) (to_int t.y) (to_int t.z))
  ;;

  let distance_sq a b =
    Float.(((a.x - b.x) ** 2.) + ((a.y - b.y) ** 2.) + ((a.z - b.z) ** 2.))
  ;;
end

let part1 input : int =
  let junction_boxes =
    String.split_lines input |> List.map ~f:Pos3.of_string |> List.to_array
  in
  let circuits = Hashtbl.create (module Pos3) ~size:(Array.length junction_boxes) in
  Array.iter junction_boxes ~f:(fun box ->
    Hashtbl.set circuits ~key:box ~data:(Hash_set.of_list (module Pos3) [ box ]));
  let distances =
    Sequence.range 0 (Array.length junction_boxes)
    |> Sequence.concat_map ~f:(fun i ->
      Sequence.range (i + 1) (Array.length junction_boxes)
      |> Sequence.map ~f:(fun j -> junction_boxes.(i), junction_boxes.(j)))
    |> Sequence.to_list
    |> List.sort ~compare:(fun (a1, b1) (a2, b2) ->
      Float.compare (Pos3.distance_sq a1 b1) (Pos3.distance_sq a2 b2))
  in
  List.iter (List.take distances 1000) ~f:(fun (a, b) ->
    (* printf "%s to %s\n" (Pos3.to_string a) (Pos3.to_string b); *)
    let circuit =
      Hash_set.union (Hashtbl.find_exn circuits a) (Hashtbl.find_exn circuits b)
    in
    (* printf
      "union: %s (size: %d)\n"
      (Sexp.to_string_hum
         (Hash_set.to_list circuit
          |> List.sort ~compare:Pos3.compare
          |> List.map ~f:Pos3.to_string
          |> List.sexp_of_t String.sexp_of_t))
      (Hash_set.length circuit); *)
    Hash_set.iter circuit ~f:(fun box -> Hashtbl.set circuits ~key:box ~data:circuit));
  let top3 =
    Hashtbl.to_alist circuits
    |> List.map ~f:snd
    |> List.sort ~compare:(fun a b ->
      -1 * Int.compare (Hash_set.length a) (Hash_set.length b))
    |> List.remove_consecutive_duplicates ~equal:phys_equal
    |> fun l -> List.take l 3
  in
  (* List.iter top3 ~f:(fun circuit ->
    printf
      "union: %s (size: %d)\n"
      (Sexp.to_string_hum
         (Hash_set.to_list circuit
          |> List.sort ~compare:Pos3.compare
          |> List.map ~f:Pos3.to_string
          |> List.sexp_of_t String.sexp_of_t))
      (Hash_set.length circuit)); *)
  List.fold (List.map top3 ~f:Hash_set.length) ~init:1 ~f:Int.( * )
;;

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
  [%expect
    {|
    part1: 20
    part2: -1
    |}];
  test Inputs.year2025_day08;
  [%expect
    {|
    part1: 79560
    part2: -1
    |}]
;;
