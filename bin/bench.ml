open Adventofocaml
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Year2025_day05.part1" (fun () ->
           ignore (Year2025_day05.part1 Inputs.year2025_day05))
       ; Bench.Test.create ~name:"Year2025_day05.part2" (fun () ->
           ignore (Year2025_day05.part2 Inputs.year2025_day05))
       ])
;;
