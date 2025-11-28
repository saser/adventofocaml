open! Base
open! Stdio

module Rule = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, hash]

    let t_of_tuple a b : t = a, b
  end

  include T
  include Comparator.Make (T)
end

module RuleSet = struct
  type t = Rule.t Hash_set.t

  let t_of_list (rules : Rule.t list) : t = Hash_set.of_list (module Rule) rules

  let compare_pages t a b =
    if Hash_set.mem t (Rule.t_of_tuple a b)
    then -1
    else if Hash_set.mem t (Rule.t_of_tuple b a)
    then 1
    else 0
  ;;
end

module Pages = struct
  type t = int list

  let t_of_list (t : int list) : t = t

  let is_sorted (t : t) ~(ruleset : RuleSet.t) =
    List.is_sorted t ~compare:(RuleSet.compare_pages ruleset)
  ;;

  let sort (t : t) ~(ruleset : RuleSet.t) =
    List.sort t ~compare:(RuleSet.compare_pages ruleset)
  ;;
end

let parse_rule line =
  match String.split (String.strip line) ~on:'|' with
  | [ before; after ] -> Rule.t_of_tuple (Int.of_string before) (Int.of_string after)
  | _ -> failwith @@ "invalind line: " ^ line
;;

let parse_pages line =
  String.strip line
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
  |> Pages.t_of_list
;;

let parse input =
  let sepidx =
    String.substr_index input ~pattern:"\n\n"
    |> Option.value_exn ~message:"no fragment separator found"
  in
  let ruleset =
    String.prefix input sepidx
    |> String.split_lines
    |> List.map ~f:parse_rule
    |> RuleSet.t_of_list
  in
  let pages =
    String.drop_prefix input (sepidx + String.length "\n\n")
    |> String.split_lines
    |> List.map ~f:parse_pages
  in
  ruleset, pages
;;

let part1 input =
  let ruleset, pages = parse input in
  List.filter pages ~f:(Pages.is_sorted ~ruleset)
  |> List.map ~f:Array.of_list
  |> List.sum (module Int) ~f:(fun page -> Array.get page (Array.length page / 2))
;;

let part2 input =
  let ruleset, pages = parse input in
  List.filter pages ~f:(Fn.non @@ Pages.is_sorted ~ruleset)
  |> List.map ~f:(List.sort ~compare:(RuleSet.compare_pages ruleset))
  |> List.map ~f:Array.of_list
  |> List.sum (module Int) ~f:(fun page -> Array.get page (Array.length page / 2))
;;

let%expect_test "parse_rule" =
  let test line =
    let before, after = parse_rule line in
    printf "%d before %d\n" before after
  in
  (* Sample from the real input. *)
  let examples =
    [ "88|54"
    ; "65|71"
    ; "65|83"
    ; "11|57"
    ; "11|36"
    ; "11|66"
    ; "83|67"
    ; "83|98"
    ; "83|88"
    ; "83|99"
    ]
  in
  List.iter examples ~f:test;
  [%expect
    {|
    88 before 54
    65 before 71
    65 before 83
    11 before 57
    11 before 36
    11 before 66
    83 before 67
    83 before 98
    83 before 88
    83 before 99
    |}]
;;

let%expect_test "parse_pages" =
  let test line =
    parse_pages line
    |> List.sexp_of_t Int.sexp_of_t
    |> Sexp.to_string_hum
    |> print_endline
  in
  (* Sample from the real input. *)
  let examples =
    [ "98,22,35,93,25,67,78,13,75,21,18,51,17,33,55,45,64,31,96,29,65"
    ; "41,43,66,36,91,15,98,25,67,78,75"
    ; "66,91,22,15,18,98,76,13,78,42,93,67,95,35,17,33,36,25,59,12,21,75,57"
    ; "89,83,99,54,41,57,66,59,91,42,95,15,98,93,25"
    ; "87,11,92,46,89,83,53,81,57,66,91"
    ; "78,75,51,64,65,16,71"
    ]
  in
  List.iter examples ~f:test;
  [%expect
    {|
    (98 22 35 93 25 67 78 13 75 21 18 51 17 33 55 45 64 31 96 29 65)
    (41 43 66 36 91 15 98 25 67 78 75)
    (66 91 22 15 18 98 76 13 78 42 93 67 95 35 17 33 36 25 59 12 21 75 57)
    (89 83 99 54 41 57 66 59 91 42 95 15 98 93 25)
    (87 11 92 46 89 83 53 81 57 66 91)
    (78 75 51 64 65 16 71)
    |}]
;;

let%expect_test "parse" =
  let test input =
    let ruleset, pages = parse input in
    print_endline "rules:";
    Hash_set.to_list ruleset
    |> List.sort ~compare:Rule.compare
    |> List.iter ~f:(fun (before, after) -> printf "%d before %d\n" before after);
    print_endline "";
    print_endline "pages: ";
    List.iter pages ~f:(fun page ->
      List.sexp_of_t Int.sexp_of_t page |> Sexp.to_string_hum |> print_endline)
  in
  (* Sample of the real input. *)
  test
    (String.strip
       {|
57|17
57|91
67|29

98,22,35,93,25,67,78,13,75,21,18,51,17,33,55,45,64,31,96,29,65
41,43,66,36,91,15,98,25,67,78,75
66,91,22,15,18,98,76,13,78,42,93,67,95,35,17,33,36,25,59,12,21,75,57
  |});
  [%expect
    {|
    rules:
    57 before 17
    57 before 91
    67 before 29

    pages:
    (98 22 35 93 25 67 78 13 75 21 18 51 17 33 55 45 64 31 96 29 65)
    (41 43 66 36 91 15 98 25 67 78 75)
    (66 91 22 15 18 98 76 13 78 42 93 67 95 35 17 33 36 25 59 12 21 75 57)
    |}]
;;

let%expect_test "page_sorted" =
  let input =
    String.strip
      {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  let ruleset, pages = parse input in
  List.iter pages ~f:(fun page ->
    printf
      "%s -> %b\n"
      (List.sexp_of_t Int.sexp_of_t page |> Sexp.to_string_hum)
      (Pages.is_sorted page ~ruleset));
  [%expect
    {|
    (75 47 61 53 29) -> true
    (97 61 53 29 13) -> true
    (75 29 13) -> true
    (75 97 47 61 53) -> false
    (61 13 29) -> false
    (97 13 75 29 47) -> false
    |}]
;;

let%expect_test "part1" =
  let test input = part1 input |> printf "%d\n" in
  let example_input =
    String.strip
      {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  test example_input;
  [%expect {| 143 |}];
  test Inputs.year2024_day05;
  [%expect {| 5588 |}]
;;

let%expect_test "part2" =
  let test input = part2 input |> printf "%d\n" in
  let example_input =
    String.strip
      {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  in
  test example_input;
  [%expect {| 123 |}];
  test Inputs.year2024_day05;
  [%expect {| 5331 |}]
;;
