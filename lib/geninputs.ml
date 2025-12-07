open Base
open Stdio

let () =
  let files =
    Sys_unix.readdir "inputs" |> Array.filter ~f:(fun file -> Char.(file.[0] <> '.'))
  in
  Array.sort files ~compare:String.compare;
  print_endline "open! Base";
  Array.iter files ~f:(fun filename ->
    printf
      "let %s = String.strip ~drop:(Char.( = ) '\\n') {xxx|%s|xxx}\n\n"
      filename
      (In_channel.read_all ("inputs/" ^ filename)))
;;
