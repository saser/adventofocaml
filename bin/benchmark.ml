open Adventofocaml
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"year2024_day01_part1" (fun () ->
           ignore @@ Year2024_day01.part1 Inputs.year2024_day01)
       ; Bench.Test.create ~name:"year2024_day01_part2" (fun () ->
           ignore @@ Year2024_day01.part2 Inputs.year2024_day01)
       ; Bench.Test.create ~name:"year2024_day02_part1" (fun () ->
           ignore @@ Year2024_day02.part1 Inputs.year2024_day02)
       ; Bench.Test.create ~name:"year2024_day02_part2" (fun () ->
           ignore @@ Year2024_day02.part2 Inputs.year2024_day02)
       ; Bench.Test.create ~name:"year2025_day01_part1" (fun () ->
           ignore @@ Year2025_day01.part1 Inputs.year2025_day01)
       ; Bench.Test.create ~name:"year2025_day01_part2" (fun () ->
           ignore @@ Year2025_day01.part2 Inputs.year2025_day01)
       ])
;;
