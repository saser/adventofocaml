open Adventofocaml
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"year2024_day01_part1" (fun () ->
           ignore Year2024_day01.(part1 full_input))
       ; Bench.Test.create ~name:"year2024_day01_part2" (fun () ->
           ignore Year2024_day01.(part2 full_input))
       ])
;;
