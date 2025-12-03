open Adventofocaml
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ (* Start of 2024. *)
         Bench.Test.create ~name:"year2024_day01_part1" (fun () ->
           ignore @@ Year2024_day01.part1 Inputs.year2024_day01)
       ; Bench.Test.create ~name:"year2024_day01_part2" (fun () ->
           ignore @@ Year2024_day01.part2 Inputs.year2024_day01)
       ; Bench.Test.create ~name:"year2024_day02_part1" (fun () ->
           ignore @@ Year2024_day02.part1 Inputs.year2024_day02)
       ; Bench.Test.create ~name:"year2024_day02_part2" (fun () ->
           ignore @@ Year2024_day02.part2 Inputs.year2024_day02)
       ; Bench.Test.create ~name:"year2024_day05_part1" (fun () ->
           ignore @@ Year2024_day05.part1 Inputs.year2024_day05)
       ; Bench.Test.create ~name:"year2024_day05_part2" (fun () ->
           ignore @@ Year2024_day05.part2 Inputs.year2024_day05)
       ; Bench.Test.create ~name:"year2024_day07_part1" (fun () ->
           ignore @@ Year2024_day07.part1 Inputs.year2024_day07)
       ; Bench.Test.create ~name:"year2024_day07_part2" (fun () ->
           ignore @@ Year2024_day07.part2 Inputs.year2024_day07)
         (* Start of 2025. *)
       ; Bench.Test.create ~name:"year2025_day01_part1" (fun () ->
           ignore @@ Year2025_day01.part1 Inputs.year2025_day01)
       ; Bench.Test.create ~name:"year2025_day01_part2" (fun () ->
           ignore @@ Year2025_day01.part2 Inputs.year2025_day01)
       ; Bench.Test.create ~name:"year2025_day02_part1" (fun () ->
           ignore @@ Year2025_day02.part1 Inputs.year2025_day02)
       ; Bench.Test.create ~name:"year2025_day02_part2" (fun () ->
           ignore @@ Year2025_day02.part2 Inputs.year2025_day02)
       ; Bench.Test.create ~name:"year2025_day03_part1" (fun () ->
           ignore @@ Year2025_day03.part1 Inputs.year2025_day03)
       ; Bench.Test.create ~name:"year2025_day03_part2" (fun () ->
           ignore @@ Year2025_day03.part2 Inputs.year2025_day03)
       ])
;;
