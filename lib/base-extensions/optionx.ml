open Base
open Stdio

let to_string_hum to_string t =
  match t with
  | None -> "None"
  | Some x -> "Some " ^ to_string x
;;

let%expect_test "to_string_hum" =
  let examples = [ None; Some 1; Some (-123) ] in
  List.iter examples ~f:(fun opt -> to_string_hum Int.to_string_hum opt |> print_endline);
  [%expect {|
    None
    Some 1
    Some -123
    |}]
;;
