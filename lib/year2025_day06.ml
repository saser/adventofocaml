open Base
open Stdio

module Matrix = struct
  type 'a t = 'a array array

  let create ~nrows ~ncols v : 'a t = Array.make_matrix ~dimx:nrows ~dimy:ncols v

  let init ~nrows ~ncols ~f : 'a t =
    Array.init nrows ~f:(fun row -> Array.init ncols ~f:(fun col -> f row col))
  ;;

  let nrows (t : 'a t) = Array.length t
  let ncols (t : 'a t) = Array.length t.(0)
  let transpose (t : 'a t) : 'a t = Array.transpose_exn t

  let sub (t : 'a t) ~row ~nrows ~col ~ncols : 'a t =
    init ~nrows ~ncols ~f:(fun rowi coli -> t.(row + rowi).(col + coli))
  ;;
end

module Worksheet = struct
  type t = char Matrix.t

  let of_string input : t =
    (* Parse the input as individual characters and pad with spaces to form a
       rectangular matrix. *)
    let chars =
      String.split_lines input |> Array.of_list |> Array.map ~f:String.to_array
    in
    let nrows = Array.length chars in
    let ncols =
      Array.map chars ~f:Array.length
      |> Array.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let m = Matrix.create ~nrows ~ncols ' ' in
    Array.iteri chars ~f:(fun row chars ->
      Array.iteri chars ~f:(fun col c -> m.(row).(col) <- c));
    m
  ;;

  let op (t : t) col =
    match t.(Matrix.nrows t - 1).(col) with
    | '+' -> ( + ), 0
    | '*' -> ( * ), 1
    | c ->
      Printf.failwithf
        "no operation found on last line at column %d; got character '%c'"
        col
        c
        ()
  ;;

  let solve (t : t) ~transpose =
    (* Assume that the operators are on the leftmost column of each problem.
       From that find the problem region by the position of the operator and the
       number of columns until the next operator. *)
    let opcols =
      Array.filter_mapi (Array.last t) ~f:(fun col c ->
        Option.some_if Char.(c <> ' ') col)
    in
    let problems =
      Array.init (Array.length opcols) ~f:(fun i ->
        let pos = opcols.(i) in
        let next =
          if i < Array.length opcols - 1
          then
            (* The -1 is to account for the column of all spaces just before the
               next operator. *)
            opcols.(i + 1) - 1
          else Matrix.ncols t
        in
        pos, next - pos)
    in
    (* Iterate over all problems, calculate their solutions, and sum the
       solutions together. *)
    let sum = ref 0 in
    for i = 0 to Array.length problems - 1 do
      let pos, len = problems.(i) in
      (* [digits] is a matrix consisting only of the spaces and digits.
         Optionally transpose it; that's how we read right-to-left,
         top-to-bottom. *)
      let digits =
        Matrix.sub t ~row:0 ~nrows:(Matrix.nrows t - 1) ~col:pos ~ncols:len
        |> fun m -> if transpose then Matrix.transpose m else m
      in
      (* Turn each row of digits into a number the usual way. Treat spaces as if
         they were leading zeroes. *)
      let numbers =
        Array.map
          digits
          ~f:
            (Array.fold ~init:0 ~f:(fun acc c ->
               match c with
               | ' ' -> acc
               | n -> (acc * 10) + Char.to_int n - Char.to_int '0'))
        |> Array.filter ~f:Int.is_positive
      in
      (* Now simply sum/multiply the numbers together and add them to the
         running sum. *)
      let f, init = op t pos in
      sum := !sum + Array.fold numbers ~init ~f
    done;
    !sum
  ;;
end

let solve input ~transpose = Worksheet.of_string input |> Worksheet.solve ~transpose
let part1 = solve ~transpose:false
let part2 = solve ~transpose:true

let example_input =
  String.strip
    ~drop:(Char.( = ) '\n')
    {|
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
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
    part1: 4277556
    part2: 3263827
    |}];
  test Inputs.year2025_day06;
  [%expect
    {|
    part1: 6172481852142
    part2: 10188206723429
    |}]
;;
