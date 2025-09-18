open Adventofocaml
open Core_bench

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"sum_10k" (fun () -> ignore Hello_benchmarks.sum_10k) ])
;;
