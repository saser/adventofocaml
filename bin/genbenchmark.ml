open Base
open Stdio

(* This program reads the list of OCaml modules from the `lib`
   folder; assumes they have functions called `part1` and `part2`; assumes a
   corresponding input file exists; and generates the code for a simple program
   that will run benchmarks for all solution functions.
   
   It's a quick and dirty way of always having up-to-date benchmarks every time
   I add a new solution. *)

let () =
  let filename_re = Re.compile (Re.Perl.re "year\\d{4}_day\\d{2}\\.ml") in
  let modules =
    Sys_unix.readdir "../lib"
    |> Array.filter ~f:(Re.execp filename_re)
    |> Array.map ~f:(fun filename ->
      String.chop_suffix_exn filename ~suffix:".ml" |> String.capitalize)
  in
  Array.sort modules ~compare:String.compare;
  printf "open Adventofocaml\n";
  printf "open Core_bench\n";
  printf "\n";
  printf "let () = Command_unix.run (Bench.make_command [\n";
  Array.iter modules ~f:(fun modname ->
    let input = "Inputs." ^ String.uncapitalize modname in
    printf
      "    Bench.Test.create ~name:\"%s.part1\" (fun () -> ignore (%s.part1 %s));\n"
      modname
      modname
      input;
    printf
      "    Bench.Test.create ~name:\"%s.part2\" (fun () -> ignore (%s.part2 %s));\n"
      modname
      modname
      input);
  printf "])\n";
  printf ";;\n"
;;
