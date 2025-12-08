open Base
open Stdio

module UnionFind = struct
  type t =
    { parent : int array
    ; size : int array
    }

  let create n =
    let parent = Array.init n ~f:Fn.id in
    let size = Array.create ~len:n 1 in
    { parent; size }
  ;;

  let find t x =
    let x = ref x in
    while t.parent.(!x) <> !x do
      t.parent.(!x) <- t.parent.(t.parent.(!x));
      x := t.parent.(!x)
    done;
    !x
  ;;

  let union t x y =
    let x = find t x in
    let y = find t y in
    if x = y
    then false
    else (
      let x, y = if t.size.(x) < t.size.(y) then y, x else x, y in
      t.parent.(y) <- x;
      t.size.(x) <- t.size.(x) + t.size.(y);
      true)
  ;;
end

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

let solve input part =
  let junction_boxes =
    String.split_lines input |> List.map ~f:Pos3.of_string |> List.to_array
  in
  let edges =
    Sequence.range 0 (Array.length junction_boxes)
    |> Sequence.concat_map ~f:(fun i ->
      Sequence.range (i + 1) (Array.length junction_boxes)
      |> Sequence.map ~f:(fun j ->
        let bi = junction_boxes.(i) in
        let bj = junction_boxes.(j) in
        Pos3.distance_sq bi bj, i, j))
    |> Sequence.to_list
    |> List.sort ~compare:(fun (d1, _, _) (d2, _, _) -> Float.compare d1 d2)
  in
  let uf = UnionFind.create (Array.length junction_boxes) in
  match part with
  | `Part1 ->
    List.iter (List.take edges 1000) ~f:(fun (_, i, j) ->
      UnionFind.union uf i j |> ignore);
    Array.sort uf.size ~compare:Int.compare;
    Array.rev_inplace uf.size;
    let answer = ref 1 in
    for i = 0 to 2 do
      answer := !answer * uf.size.(i)
    done;
    !answer
  | `Part2 ->
    List.fold_until
      edges
      ~init:()
      ~f:(fun () (_, i, j) ->
        if UnionFind.union uf i j
        then
          if uf.size.(UnionFind.find uf i) = Array.length junction_boxes
          then Stop (Float.to_int junction_boxes.(i).x * Float.to_int junction_boxes.(j).x)
          else Continue ()
        else Continue ())
      ~finish:(fun () -> -2)
;;

let part1 input = solve input `Part1
let part2 input = solve input `Part2

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
    part1: 500
    part2: 25272
    |}];
  test Inputs.year2025_day08;
  [%expect {|
    part1: 79560
    part2: 31182420
    |}]
;;
