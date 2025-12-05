open Base

let split s ~sep =
  let rec loop s pos =
    let open Sequence.Generator in
    match String.substr_index s ~pos ~pattern:sep with
    | None -> yield (String.subo s ~pos) >>= fun () -> return ()
    | Some i ->
      yield (String.sub s ~pos ~len:(i - pos)) >>= fun () -> loop s (i + String.length sep)
  in
  loop s 0 |> Sequence.Generator.run
;;

let split_lines s = split s ~sep:"\n"

let%expect_test "split" =
  let examples =
    [ "", " "
    ; " ", " "
    ; "a", " "
    ; "a b c d", " "
    ; "a\nb\nc\nd", "\n"
    ; "a\nb\nc\nd\n", "\n"
    ; "a_foo_b_foo_c_bar_", "_foo_"
    ]
  in
  Ascii_table.simple_list_table
    [ "s"; "sep"; "split s ~sep" ]
    (List.map examples ~f:(fun (s, sep) ->
       let quote = Printf.sprintf "%S" in
       [ quote s
       ; quote sep
       ; Sexp.to_string_hum [%sexp (split s ~sep |> Sequence.to_list : string list)]
       ]));
  [%expect
    {|
    ┌──────────────────────┬─────────┬──────────────┐
    │                    s │     sep │ split s ~sep │
    ├──────────────────────┼─────────┼──────────────┤
    │                   "" │     " " │         ("") │
    │                  " " │     " " │      ("" "") │
    │                  "a" │     " " │          (a) │
    │            "a b c d" │     " " │    (a b c d) │
    │         "a\nb\nc\nd" │    "\n" │    (a b c d) │
    │       "a\nb\nc\nd\n" │    "\n" │ (a b c d "") │
    │ "a_foo_b_foo_c_bar_" │ "_foo_" │ (a b c_bar_) │
    └──────────────────────┴─────────┴──────────────┘
    |}]
;;
