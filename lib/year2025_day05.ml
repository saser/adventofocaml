open Base
open Stdio

module Range = struct
  type t =
    { low : int
    ; high : int
    }
  [@@deriving sexp]

  let parse (s : string) : t =
    match String.split s ~on:'-' with
    | [ low; high ] -> { low = Int.of_string low; high = Int.of_string high }
    | _ -> Printf.failwithf "invalid range: %S" s ()
  ;;

  let to_string t = Printf.sprintf "%d-%d" t.low t.high

  let compare x y =
    [ Int.compare x.low y.low; Int.compare x.high y.high ]
    |> List.find ~f:(( <> ) 0)
    |> Option.value ~default:0
  ;;

  let contains t i = Int.between i ~low:t.low ~high:t.high
  let overlaps x y = not (Int.max x.low y.low > Int.min x.high y.high)
end

let part1 input =
  let fragments = String.Search_pattern.(split_on (create "\n\n") input) in
  let ranges =
    List.nth_exn fragments 0
    |> String.split_lines
    |> List.map ~f:Range.parse
    |> List.sort ~compare:Range.compare
  in
  let numbers =
    List.nth_exn fragments 1 |> String.split_lines |> List.map ~f:Int.of_string
  in
  let ranges_contains i =
    List.find ranges ~f:(fun range -> Range.contains range i) |> Option.is_some
  in
  List.count numbers ~f:ranges_contains
;;

let count_ids ranges =
  List.group ranges ~break:(fun x y -> not (Range.overlaps x y))
  |> List.sum
       (module Int)
       ~f:(fun g ->
         print_s [%sexp (g : Range.t list)];
         let low = (List.hd_exn g).low in
         let high =
           (List.max_elt g ~compare:(fun x y -> Int.compare x.high y.high)
            |> Option.value_exn)
             .high
         in
         let sum = high - low + 1 in
         printf "-> %d\n" sum;
         sum)
;;

let part2 input =
  let fragments = String.Search_pattern.(split_on (create "\n\n") input) in
  let ranges =
    List.nth_exn fragments 0
    |> String.split_lines
    |> List.map ~f:Range.parse
    |> List.sort ~compare:Range.compare
  in
  let groups = List.group ranges ~break:(fun x y -> not (Range.overlaps x y)) in
  List.sum
    (module Int)
    groups
    ~f:(fun g ->
      (* print_s [%sexp (g : Range.t list)]; *)
      let low = (List.hd_exn g).low in
      let high =
        (List.max_elt g ~compare:(fun x y -> Int.compare x.high y.high)
         |> Option.value_exn)
          .high
      in
      let sum = high - low + 1 in
      (* printf "-> %d\n" sum; *)
      sum)
;;

let example_input =
  String.strip
    {|
3-5
10-14
16-20
12-18

1
5
8
11
17
32
|}
;;

let%expect_test "Range.parse" =
  let examples =
    [ (* From example input. *)
      "3-5"
    ; "10-14"
    ; "16-20"
    ; "12-18"
    ; (* Sample from real input. *)
      "5086738007739-6875107477983"
    ; "342866708370012-346965358558799"
    ; "185250814640704-186019238615676"
    ; "555804456885533-560879776586056"
    ]
  in
  Ascii_table.simple_list_table
    [ "s"; "Range.parse s" ]
    (List.map examples ~f:(fun s ->
       [ s; Range.sexp_of_t (Range.parse s) |> Sexp.to_string_hum ]));
  [%expect
    {|
    ┌─────────────────────────────────┬────────────────────────────────────────────────┐
    │                               s │                                  Range.parse s │
    ├─────────────────────────────────┼────────────────────────────────────────────────┤
    │                             3-5 │                             ((low 3) (high 5)) │
    │                           10-14 │                           ((low 10) (high 14)) │
    │                           16-20 │                           ((low 16) (high 20)) │
    │                           12-18 │                           ((low 12) (high 18)) │
    │     5086738007739-6875107477983 │     ((low 5086738007739) (high 6875107477983)) │
    │ 342866708370012-346965358558799 │ ((low 342866708370012) (high 346965358558799)) │
    │ 185250814640704-186019238615676 │ ((low 185250814640704) (high 186019238615676)) │
    │ 555804456885533-560879776586056 │ ((low 555804456885533) (high 560879776586056)) │
    └─────────────────────────────────┴────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "sorting Ranges" =
  let test input =
    let unsorted = String.split_lines input |> List.map ~f:Range.parse in
    let sorted = List.sort unsorted ~compare:Range.compare in
    Ascii_table.simple_list_table
      [ "unsorted"; "sorted" ]
      (List.zip_exn unsorted sorted
       |> List.map ~f:(fun (unsorted, sorted) ->
         [ unsorted; sorted ] |> List.map ~f:Range.to_string))
  in
  test (String.Search_pattern.(split_on (create "\n\n") example_input) |> List.hd_exn);
  [%expect
    {|
    ┌──────────┬────────┐
    │ unsorted │ sorted │
    ├──────────┼────────┤
    │      3-5 │    3-5 │
    │    10-14 │  10-14 │
    │    16-20 │  12-18 │
    │    12-18 │  16-20 │
    └──────────┴────────┘
    |}]
;;

let%expect_test "Range.contains" =
  let r = Range.parse "12-18" in
  Ascii_table.simple_list_table
    [ "i"; "(12-18).contains i" ]
    (List.range ~start:`inclusive ~stop:`inclusive 0 20
     |> List.map ~f:(fun i -> [ Int.to_string i; Bool.to_string (Range.contains r i) ]));
  [%expect
    {|
    ┌────┬────────────────────┐
    │  i │ (12-18).contains i │
    ├────┼────────────────────┤
    │  0 │              false │
    │  1 │              false │
    │  2 │              false │
    │  3 │              false │
    │  4 │              false │
    │  5 │              false │
    │  6 │              false │
    │  7 │              false │
    │  8 │              false │
    │  9 │              false │
    │ 10 │              false │
    │ 11 │              false │
    │ 12 │               true │
    │ 13 │               true │
    │ 14 │               true │
    │ 15 │               true │
    │ 16 │               true │
    │ 17 │               true │
    │ 18 │               true │
    │ 19 │              false │
    │ 20 │              false │
    └────┴────────────────────┘
    |}]
;;

let%expect_test "Range.overlaps" =
  let test input =
    let ranges = String.split_lines input |> List.map ~f:Range.parse in
    Ascii_table.simple_list_table
      [ "x"; "y"; "overlaps x y" ]
      (List.Cartesian_product.map2 ranges ranges ~f:(fun x y ->
         [ Range.to_string x; Range.to_string y; Range.overlaps x y |> Bool.to_string ]))
  in
  test
    (String.strip
       {|
3-5
10-14
16-20
12-18
    |});
  [%expect
    {|
    ┌───────┬───────┬──────────────┐
    │     x │     y │ overlaps x y │
    ├───────┼───────┼──────────────┤
    │   3-5 │   3-5 │         true │
    │   3-5 │ 10-14 │        false │
    │   3-5 │ 16-20 │        false │
    │   3-5 │ 12-18 │        false │
    │ 10-14 │   3-5 │        false │
    │ 10-14 │ 10-14 │         true │
    │ 10-14 │ 16-20 │        false │
    │ 10-14 │ 12-18 │         true │
    │ 16-20 │   3-5 │        false │
    │ 16-20 │ 10-14 │        false │
    │ 16-20 │ 16-20 │         true │
    │ 16-20 │ 12-18 │         true │
    │ 12-18 │   3-5 │        false │
    │ 12-18 │ 10-14 │         true │
    │ 12-18 │ 16-20 │         true │
    │ 12-18 │ 12-18 │         true │
    └───────┴───────┴──────────────┘
    |}]
;;

let%expect_test "debug" =
  let examples =
    [ "1-5", "5-10"
    ; "1-5", "6-10"
    ; "1-10", "4-5"
    ; "1-7", "4-10"
    ; "1-7", "7-10"
    ; "1-7", "8-10"
    ; "1-10", "1-1"
    ; "1-10", "1-10"
    ; "1-10", "2-10"
    ; "1-10", "10-10"
    ; "9-16", "10-15"
    ]
  in
  Ascii_table.simple_list_table
    [ "x"; "y"; "count_ids [x; y]" ]
    (List.map examples ~f:(fun (x, y) ->
       [ x; y; [ x; y ] |> List.map ~f:Range.parse |> count_ids |> Int.to_string ]));
  [%expect
    {|
    (((low 1) (high 5)) ((low 5) (high 10)))
    -> 10
    (((low 1) (high 5)))
    -> 5
    (((low 6) (high 10)))
    -> 5
    (((low 1) (high 10)) ((low 4) (high 5)))
    -> 10
    (((low 1) (high 7)) ((low 4) (high 10)))
    -> 10
    (((low 1) (high 7)) ((low 7) (high 10)))
    -> 10
    (((low 1) (high 7)))
    -> 7
    (((low 8) (high 10)))
    -> 3
    (((low 1) (high 10)) ((low 1) (high 1)))
    -> 10
    (((low 1) (high 10)) ((low 1) (high 10)))
    -> 10
    (((low 1) (high 10)) ((low 2) (high 10)))
    -> 10
    (((low 1) (high 10)) ((low 10) (high 10)))
    -> 10
    (((low 9) (high 16)) ((low 10) (high 15)))
    -> 8
    ┌──────┬───────┬──────────────────┐
    │    x │     y │ count_ids [x; y] │
    ├──────┼───────┼──────────────────┤
    │  1-5 │  5-10 │               10 │
    │  1-5 │  6-10 │               10 │
    │ 1-10 │   4-5 │               10 │
    │  1-7 │  4-10 │               10 │
    │  1-7 │  7-10 │               10 │
    │  1-7 │  8-10 │               10 │
    │ 1-10 │   1-1 │               10 │
    │ 1-10 │  1-10 │               10 │
    │ 1-10 │  2-10 │               10 │
    │ 1-10 │ 10-10 │               10 │
    │ 9-16 │ 10-15 │                8 │
    └──────┴───────┴──────────────────┘
    |}];
  let example_inputs =
    [ String.strip
        {|
3-5
10-14
16-20
12-18
|}
    ; String.strip
        {|
3-5
3-5
10-14
16-20
10-14
12-18
10-14
|}
    ]
  in
  List.iter example_inputs ~f:(fun input ->
    print_endline input;
    let ids =
      String.split_lines input
      |> List.map ~f:Range.parse
      |> List.sort ~compare:Range.compare
      |> count_ids
    in
    printf "-> %d\n" ids);
  [%expect
    {|
    3-5
    10-14
    16-20
    12-18
    (((low 3) (high 5)))
    -> 3
    (((low 10) (high 14)) ((low 12) (high 18)) ((low 16) (high 20)))
    -> 11
    -> 14
    3-5
    3-5
    10-14
    16-20
    10-14
    12-18
    10-14
    (((low 3) (high 5)) ((low 3) (high 5)))
    -> 3
    (((low 10) (high 14)) ((low 10) (high 14)) ((low 10) (high 14))
     ((low 12) (high 18)) ((low 16) (high 20)))
    -> 11
    -> 14
    |}]
;;

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test
  @@ String.strip
       {|
3-5
10-14
16-20
12-18
1-6

1
  |};
  [%expect
    {|
    part1: 1
    part2: 17
    |}];
  test example_input;
  [%expect
    {|
    part1: 3
    part2: 14
    |}];
  test Inputs.year2025_day05;
  [%expect
    {|
    part1: 848
    part2: 334714395325711
    |}]
;;
