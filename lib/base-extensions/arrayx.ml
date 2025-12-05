open Base

let max_idx ?(pos = 0) ?len t ~compare =
  let loop t ~pos ~len ~compare =
    let max = ref None in
    for i = pos to pos + len - 1 do
      match !max with
      | None -> max := Some i
      | Some j -> if compare t.(i) t.(j) > 0 then max := Some i
    done;
    !max
  in
  loop t ~pos ~len:(Option.value len ~default:(Array.length t)) ~compare
;;

let%expect_test "max_idx with defaults" =
  let examples =
    [ [||]
    ; [| 1 |]
    ; [| 9; 1 |]
    ; [| 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 9 |]
    ; [| 9; 9; 9; 9; 9; 9; 9; 9; 9; 9; 10 |]
    ]
  in
  Ascii_table.simple_list_table
    [ "t"; "max_idx t ~compare:Int.compare" ]
    (List.map examples ~f:(fun t ->
       [ Sexp.to_string_hum [%sexp (t : int array)]
       ; Optionx.to_string_hum Int.to_string (max_idx t ~compare:Int.compare)
       ]));
  [%expect
    {|
    ┌──────────────────────────┬────────────────────────────────┐
    │                        t │ max_idx t ~compare:Int.compare │
    ├──────────────────────────┼────────────────────────────────┤
    │                       () │                           None │
    │                      (1) │                         Some 0 │
    │                    (9 1) │                         Some 0 │
    │  (9 9 9 9 9 9 9 9 9 9 9) │                         Some 0 │
    │ (9 9 9 9 9 9 9 9 9 9 10) │                        Some 10 │
    └──────────────────────────┴────────────────────────────────┘
    |}];
  let t = [| 1; 2; 3; 4; 5; 6; 7 |] in
  let args : (pos:int * len:int) list = [] in
  printf "lol\n"
;;
