open Infinite_values_code
open List_code

let testList = list_cons 2_500 (list_cons 500_000 list_nil)
let testList2 = list_cons 500 (list_cons 1_000 list_nil)
let testList3 = list_cons 12_000 (list_cons 2_500 (list_cons 500_000 list_nil))

let testPrefix =
  Printf.printf "Testing prefix of testList\nResult = [";
  list_iter (Printf.printf "%d,") (prefix testList 4);
  Printf.printf "]\n";
  Printf.printf "Expect = [2500,500000,0,0,0,]\n"

let testSubtractPositions = 
  Printf.printf "\nTestineg subtract_positions on list 1 and 2\n";
  Printf.printf "Result = [";
  list_iter (Printf.printf "%d,") (subtract_positions testList testList2);
  Printf.printf "]\n";
  Printf.printf "Expect = [2000,499000,]\n"

  let testSubtractPositions2 = 
    Printf.printf "\nTestineg subtract_positions on list 1 and 3\n";
    Printf.printf "Result = [";
    list_iter (Printf.printf "%d,") (subtract_positions testList testList3);
    Printf.printf "]\n";
    Printf.printf "Expect = [-12000,0,0,]\n"

    let testSubtractPositions3 = 
      Printf.printf "\nTestineg subtract_positions on list 3 and 1\n";
      Printf.printf "Result = [";
      list_iter (Printf.printf "%d,") (subtract_positions testList3 testList);
      Printf.printf "]\n";
      Printf.printf "Expect = [12000,0,0,]\n"