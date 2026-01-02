open Base
open Stdio

module Button = struct
  type t = int

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '(' || c = ')'))
    |> String.split ~on:','
    |> List.fold ~init:0 ~f:(fun button n -> Int.(button lor (1 lsl of_string n)))
  ;;

  let to_list t =
    let rec loop acc i t =
      match t with
      | 0 -> List.rev acc
      | n -> loop (if n % 2 = 1 then i :: acc else acc) (i + 1) (Int.shift_right n 1)
    in
    loop [] 0 t
  ;;

  let to_string t =
    to_list t
    |> List.map ~f:Int.to_string
    |> String.concat ~sep:","
    |> fun s -> "(" ^ s ^ ")"
  ;;
end

module Lights = struct
  type t =
    { n : int
    ; len : int
    }
  [@@deriving compare, equal, hash, sexp_of]

  let of_string s =
    let s = String.strip s ~drop:(fun c -> Char.(c = '[' || c = ']')) in
    let n =
      String.foldi s ~init:0 ~f:(fun i lights c ->
        let b = Char.(c = '#') |> Bool.to_int in
        Int.(lights lor (b lsl i)))
    in
    { n; len = String.length s }
  ;;

  let to_string t =
    let chars = Array.create ~len:t.len '.' in
    for b = 0 to t.len - 1 do
      if Int.(t.n land (1 lsl b)) <> 0 then chars.(b) <- '#'
    done;
    "[" ^ String.of_array chars ^ "]"
  ;;

  let apply t button = { t with n = Int.(t.n lxor button) }
  let start t = { t with n = 0 }
end

module Joltage : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
  val start : t -> t
  val apply : t -> Button.t -> t

  include Equal.S with type t := t
  include Hashable.Key with type t := t
end = struct
  type t = int list [@@deriving hash, compare, equal, sexp_of]

  let of_string s =
    String.strip s ~drop:(fun c -> Char.(c = '{' || c = '}'))
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
  ;;

  let to_string t =
    List.map t ~f:Int.to_string |> String.concat ~sep:"," |> fun s -> "{" ^ s ^ "}"
  ;;

  let start t = List.init (List.length t) ~f:(fun _ -> 0)

  let apply t button =
    let values = List.to_array t in
    List.iter (Button.to_list button) ~f:(fun n -> values.(n) <- values.(n) + 1);
    List.of_array values
  ;;
end

let fewest_light_presses target buttons =
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
        (* else printf "have already seen %s; skipping it\n" (Lights.to_string lights') *)))
  done;
  Option.value !answer ~default:(-1)
;;

let rec combinations = function
  | [] -> [ [] ]
  | x :: tl ->
    let rest = combinations tl in
    List.append rest (List.map rest ~f:(fun c -> x :: c))
;;

let odd_candidates target buttons =
  let test combination =
    let result = List.fold combination ~init:(Lights.start target) ~f:Lights.apply in
    Option.some_if (Lights.equal result target) combination
  in
  List.filter_map (combinations buttons) ~f:test
;;

let fewest_joltage_presses2 _target _buttons = -1

let fewest_joltage_presses target buttons =
  let start = Joltage.start target in
  let seen = Hashtbl.of_alist_exn (module Joltage) [ start, 0 ] in
  let q = Queue.of_list [ start ] in
  let answer = ref None in
  while (not (Queue.is_empty q)) && Option.is_none !answer do
    let joltage = Queue.dequeue_exn q in
    let count = Hashtbl.find_exn seen joltage in
    (* printf "dequeued %s, %d\n" (Lights.to_string lights) count; *)
    if Joltage.equal joltage target
    then
      (* printf "reached target %s in %d steps!\n" (Lights.to_string target) count; *)
      answer := Some count
    else (
      let count' = count + 1 in
      List.iter buttons ~f:(fun button ->
        let lights' = Joltage.apply joltage button in
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
  |> List.sum
       (module Int)
       ~f:(fun (lights, buttons) -> fewest_light_presses lights buttons)
;;

let part2 input =
  String.split_lines input
  |> List.map ~f:(fun line ->
    let fields = String.split line ~on:' ' in
    let joltage = List.last_exn fields |> Joltage.of_string in
    let buttons =
      List.tl_exn (List.drop_last_exn fields) |> List.map ~f:Button.of_string
    in
    joltage, buttons)
  |> List.sum
       (module Int)
       ~f:(fun (lights, buttons) -> fewest_joltage_presses lights buttons)
;;

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
    part2: 33
    |}];
  (* test Inputs.year2025_day10; *)
  [%expect {| |}]
;;

let%expect_test "combinations" =
  let test xs =
    print_endline "xs:";
    print_s [%sexp (xs : int list)];
    print_endline "combinations:";
    List.iter (combinations xs) ~f:(fun c -> print_s [%sexp (c : int list)])
  in
  test [];
  [%expect
    {|
    xs:
    ()
    combinations:
    ()
    |}];
  test [ 1 ];
  [%expect
    {|
    xs:
    (1)
    combinations:
    ()
    (1)
    |}];
  test [ 1; 2; 3 ];
  [%expect
    {|
    xs:
    (1 2 3)
    combinations:
    ()
    (3)
    (2)
    (2 3)
    (1)
    (1 3)
    (1 2)
    (1 2 3)
    |}]
;;

let%expect_test "odd_candidates" =
  let test target buttons =
    let target = Lights.of_string target in
    let buttons = String.split buttons ~on:' ' |> List.map ~f:Button.of_string in
    printf "target: %s\n" (Lights.to_string target);
    printf "buttons: %s\n" (String.concat ~sep:" " (List.map buttons ~f:Button.to_string));
    print_endline "odd_candidates:";
    List.iter (odd_candidates target buttons) ~f:(fun candidate ->
      String.concat ~sep:" " (List.map candidate ~f:Button.to_string) |> print_endline)
  in
  test "[##.#]" "(3) (1,3) (2) (2,3) (0,2) (0,1)";
  [%expect
    {|
    target: [##.#]
    buttons: (3) (1,3) (2) (2,3) (0,2) (0,1)
    odd_candidates:
    (2) (2,3) (0,1)
    (1,3) (2) (0,2)
    (3) (0,1)
    (3) (1,3) (2,3) (0,2)
    |}]
;;

let%expect_test "Lights.apply" =
  let lights = Lights.of_string "[...#.]" in
  let button = Button.of_string "(1,2,3,4)" in
  printf "      %10s\n" (Int.Binary.to_string_hum lights.n);
  printf "apply %10s\n" (Int.Binary.to_string_hum button);
  printf "=     %10s\n" (Int.Binary.to_string_hum (Lights.apply lights button).n);
  [%expect
    {|
              0b1000
    apply   0b1_1110
    =       0b1_0110
    |}]
;;
