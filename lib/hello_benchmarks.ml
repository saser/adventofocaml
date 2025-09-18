let sum_10k =
  let i = ref 0 in
  for j = 1 to 10_000 do
    i := !i + j
  done;
  !i
;;
