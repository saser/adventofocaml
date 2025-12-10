open Base
open Stdio

module Button : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val to_list : t -> int list
end = struct
  type t = int list

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '(' || c = ')'))
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
  ;;

  let to_string t =
    List.map t ~f:Int.to_string |> String.concat ~sep:"," |> fun s -> "(" ^ s ^ ")"
  ;;

  let to_list t = t
end

module Lights : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val apply : t -> Button.t -> t
  val start : t -> t
  val target : t -> t

  include Equal.S with type t := t
  include Hashable.Key with type t := t
end = struct
  type t = string [@@deriving compare, equal, hash, sexp_of]

  let of_string s = String.strip s ~drop:(fun c -> Char.(c = '[' || c = ']'))
  let to_string t = "[" ^ t ^ "]"

  let apply t button =
    let chars = String.to_array t in
    List.iter (Button.to_list button) ~f:(fun n ->
      chars.(n)
      <- (match chars.(n) with
          | '.' -> '#'
          | '#' -> '.'
          | c -> Printf.failwithf "invalid char %C" c ()));
    String.of_array chars
  ;;

  let fill t c = Array.create ~len:(String.length t) c |> String.of_array
  let start t = fill t '.'
  let target t = fill t '#'
end

let fewest_presses target buttons =
  let start = Lights.start target in
  let seen = Hashtbl.of_alist_exn (module Lights) [ start, 0 ] in
  let q = Queue.of_list [ start ] in
  let answer = ref None in
  while (not (Queue.is_empty q)) && Option.is_none !answer do
    let lights = Queue.dequeue_exn q in
    let count = Hashtbl.find_exn seen lights in
    (* printf "dequeued %s, %d\n" (Lights.to_string lights) count; *)
    if Lights.equal lights target
    then
      (* printf "reached target %s in %d steps!\n" (Lights.to_string target) count; *)
      answer := Some count
    else (
      let count' = count + 1 in
      List.iter buttons ~f:(fun button ->
        let lights' = Lights.apply lights button in
        (* printf
          "%s --%s--> %s\n"
          (Lights.to_string lights)
          (Button.to_string button)
          (Lights.to_string lights'); *)
        if not (Hashtbl.mem seen lights')
        then (
          (* printf
            "have not yet seen %s; queueing it with %d steps\n"
            (Lights.to_string lights')
            count'; *)
          Hashtbl.add_exn seen ~key:lights' ~data:count';
          Queue.enqueue q lights')
        else
          ( (* printf "have already seen %s; skipping it\n" (Lights.to_string lights') *) )))
  done;
  Option.value !answer ~default:(-1)
;;

let part1 input =
  String.split_lines input
  |> List.map ~f:(fun line ->
    let fields = String.split line ~on:' ' in
    let lights = List.hd_exn fields |> Lights.of_string in
    let buttons =
      List.tl_exn (List.drop_last_exn fields) |> List.map ~f:Button.of_string
    in
    lights, buttons)
  |> List.sum (module Int) ~f:(fun (lights, buttons) -> fewest_presses lights buttons)
;;

let part2 _input = -2

let example_input =
  String.strip
    {|
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
|}
;;

let%expect_test "Lights.apply" =
  let lights = ref @@ Lights.of_string "[.##.]" in
  let buttons =
    [ Button.of_string "(3)"
    ; Button.of_string "(1,3)"
    ; Button.of_string "(2)"
    ; Button.of_string "(2,3)"
    ; Button.of_string "(0,2)"
    ; Button.of_string "(0,1)"
    ]
  in
  List.iter buttons ~f:(fun button ->
    let lights' = Lights.apply !lights button in
    printf
      "%s --%s--> %s\n"
      (Lights.to_string !lights)
      (Button.to_string button)
      (Lights.to_string lights');
    lights := lights');
  [%expect
    {|
    [.##.] --(3)--> [.###]
    [.###] --(1,3)--> [..#.]
    [..#.] --(2)--> [....]
    [....] --(2,3)--> [..##]
    [..##] --(0,2)--> [#..#]
    [#..#] --(0,1)--> [.#.#]
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
    part1: 7
    part2: -2
    |}];
  test Inputs.year2025_day10;
  [%expect
    {|
    part1: 505
    part2: -2
    |}]
;;
