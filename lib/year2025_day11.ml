open Base
open Stdio

module State = struct
  type t =
    { src : string
    ; has_dac : bool
    ; has_fft : bool
    }
  [@@deriving compare, hash, sexp_of]
end

let all_paths edges ~src ~requires_dac_and_fft =
  let dst = "out" in
  let memo = Hashtbl.create (module State) in
  let rec go src has_dac has_fft =
    let (key : State.t) = { src; has_dac; has_fft } in
    match Hashtbl.find memo key with
    | Some n -> n
    | None ->
      if String.(src = dst)
      then Bool.to_int (if requires_dac_and_fft then has_dac && has_fft else true)
      else (
        let n =
          List.sum
            (module Int)
            (Hashtbl.find_exn edges src)
            ~f:(fun next ->
              go next (has_dac || String.(next = "dac")) (has_fft || String.(next = "fft")))
        in
        Hashtbl.add_exn memo ~key ~data:n;
        n)
  in
  go src false false
;;

let parse input =
  let edges = Hashtbl.create (module String) in
  String.split_lines input
  |> List.map ~f:(String.filter ~f:(Char.( <> ) ':'))
  |> List.map ~f:(String.split ~on:' ')
  |> List.iter ~f:(fun fields ->
    Hashtbl.add_exn edges ~key:(List.hd_exn fields) ~data:(List.tl_exn fields));
  edges
;;

let part1 input = all_paths (parse input) ~src:"you" ~requires_dac_and_fft:false
let part2 input = all_paths (parse input) ~src:"svr" ~requires_dac_and_fft:true

let example_input_part1 =
  String.strip
    {|
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
|}
;;

let%expect_test "part1" =
  let test input = printf "part1: %d\n" (part1 input) in
  test example_input_part1;
  [%expect {| part1: 5 |}];
  test Inputs.year2025_day11;
  [%expect {| part1: 746 |}]
;;

let example_input_part2 =
  String.strip
    {|
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
  |}
;;

let%expect_test "part2" =
  let test input = printf "part2: %d\n" (part2 input) in
  test example_input_part2;
  [%expect {| part2: 2 |}];
  test Inputs.year2025_day11;
  [%expect {| part2: 370500293582760 |}]
;;
