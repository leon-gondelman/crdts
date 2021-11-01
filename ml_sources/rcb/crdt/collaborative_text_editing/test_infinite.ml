open Infinite_values_code
open List_code

let testList = list_cons 2_500 (list_cons 500_000 list_nil)

let testPrefix =
  Printf.printf "Prefix of testList = [";
  list_iter (Printf.printf "%d,") (prefix testList 4);
  Printf.printf "]\n"