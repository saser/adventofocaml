let%expect_test "greet" =
  print_endline "Hello, expect tests!";
  [%expect {| Hello, expect tests! |}]
;;
