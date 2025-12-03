open Base

let max_idx t ~compare =
  let max = ref None in
  Array.iteri t ~f:(fun i _ ->
    match !max with
    | None -> max := Some i
    | Some j -> if compare t.(i) t.(j) > 0 then max := Some i);
  !max
;;

let%expect_test "max_idx" =
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
  [%expect {|
    ┌──────────────────────────┬────────────────────────────────┐
    │                        t │ max_idx t ~compare:Int.compare │
    ├──────────────────────────┼────────────────────────────────┤
    │                       () │                           None │
    │                      (1) │                         Some 0 │
    │                    (9 1) │                         Some 0 │
    │  (9 9 9 9 9 9 9 9 9 9 9) │                         Some 0 │
    │ (9 9 9 9 9 9 9 9 9 9 10) │                        Some 10 │
    └──────────────────────────┴────────────────────────────────┘
    |}]
;;
