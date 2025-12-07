open Base
open Stdio

let solve input part =
  let grid = String.split_lines input |> List.map ~f:String.to_array |> List.to_array in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let splits = ref 0 in
  let timelines = Array.make_matrix ~dimx:nrows ~dimy:ncols 0 in
  for col = 0 to ncols - 1 do
    if Char.(grid.(0).(col) = 'S')
    then (
      grid.(0).(col) <- '|';
      timelines.(0).(col) <- 1)
  done;
  for row = 1 to nrows - 1 do
    for col = 0 to ncols - 1 do
      if Char.(grid.(row - 1).(col) = '|')
      then (
        let x = timelines.(row - 1).(col) in
        match grid.(row).(col) with
        | '.' | '|' ->
          grid.(row).(col) <- '|';
          timelines.(row).(col) <- timelines.(row).(col) + x
        | '^' ->
          if col > 0
          then (
            grid.(row).(col - 1) <- '|';
            timelines.(row).(col - 1) <- timelines.(row).(col - 1) + x);
          if col < ncols - 1
          then (
            grid.(row).(col + 1) <- '|';
            timelines.(row).(col + 1) <- timelines.(row).(col + 1) + x);
          splits := !splits + 1
        | c -> Printf.failwithf "row %d col %d has unexpected character %c" row col c ())
    done
  done;
  match part with
  | `Part1 -> !splits
  | `Part2 -> Array.sum (module Int) (Array.last timelines) ~f:Fn.id
;;

let solve' input part =
  let lines = String.split_lines input |> List.map ~f:String.to_array |> List.to_array in
  (* The ideas in this version of the solution comes from
     https://www.reddit.com/r/adventofcode/comments/1pg9w66/comment/nspspup/ and
     the linked code,
     https://github.com/maneatingape/advent-of-code-rust/blob/f9c26f0c417493a98ccf40be0e37a071ddf79f77/src/year2025/day07.rs.
     It's a great solution that runs quite a bit faster than my solution (~120us
     vs. ~170us on my laptop). I'm keeping both around because I think this one is
     a bit harder to understand, and I guess that if I come back and read my own
     solution again in a few years, it will make more intuitive sense than this
     one.

     The width of the relevant part of each line starts at 1 (for the 'S') and
     then increases by 2 (1 to the left, 1 to the right) for each line. E.g. if
     'S' is at column x, the relevant part is [x; x]; for the next line, the
     relevant part is [x-1; x+1]; for the line after that it's [x-2; x+2]; and
     so forth.

     With this in mind, make a few assumptions about the shape of the input,
     which came from just looking at the sample and real inputs.

     1. The 'S' position is in the very middle of the first line.
     2. Every odd line consists only of '.' and can be skipped.
     3. The first and last character of each line will be '^'. Assuming 'S' is
        on x, then x-n and x+n will both be '^' for line n.

     Thanks to (3), we also only need to check every other position on each
     line. Observe:
     ....S....
     ...^.^...
     ..^.^.^..
     .^.^^..^
         z
    
     The '^' at column z cannot ever be reached, because there is a '^' in the
     line above it. This can be generalized: with assumption (3), if line n
     begins at column x, then line n-1 will have a '^' at column x+1, so there's
     no need to check line n, column x+1.

     I'm sure you can write an inductive proof or something of this, but I'm too
     lazy to do that now, so the above argument should be enough to convince
     future me when I read the code again. *)
  let width = Array.length lines.(0) in
  let center = width / 2 in
  let splits = ref 0 in
  let timelines = Array.create ~len:width 0 in
  timelines.(center) <- 1;
  Array.iteri lines ~f:(fun y line ->
    (* Skip the first two lines and then every odd line since they're assumed to
       be empty. *)
    let skip = y < 2 || y % 2 = 1 in
    if not skip
    then (
      let n = (y - 2) / 2 in
      Sequence.range
        (center - n)
        (center + n)
        ~start:`inclusive
        ~stop:`inclusive
        ~stride:2
      |> Sequence.iter ~f:(fun x ->
        let count = timelines.(x) in
        if count > 0 && Char.(line.(x) = '^')
        then (
          splits := !splits + 1;
          timelines.(x) <- 0;
          timelines.(x - 1) <- timelines.(x - 1) + count;
          timelines.(x + 1) <- timelines.(x + 1) + count))));
  match part with
  | `Part1 -> !splits
  | `Part2 -> Array.sum (module Int) timelines ~f:Fn.id
;;

let part1 input = solve' input `Part1
let part2 input = solve' input `Part2

let example_input =
  String.strip
    {|
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
|}
;;

let%expect_test "solution" =
  let test input =
    printf "part1: %d\n" (part1 input);
    printf "part2: %d\n" (part2 input)
  in
  test example_input;
  [%expect
    {|
    part1: 21
    part2: 40
    |}];
  test Inputs.year2025_day07;
  [%expect
    {|
    part1: 1605
    part2: 29893386035180
    |}]
;;
