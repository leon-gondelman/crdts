open Infinite_values_code
open List_code

let testsRun = ref 0
let testsPass = ref 0

let passTest check = 
  if check then Printf.printf "\027[1;32mTEST SUCCESS\027[0m\n-----------------------\n\n" else Printf.printf "\027[1;31mTEST FAILED\027[0m\n-----------------------\n\n"; check

let test_and_print result expected = 
  let stringRes = ref "[" in 
  list_iter (fun x -> stringRes := !stringRes ^ (Printf.sprintf "%d," x)) result;
  stringRes := !stringRes ^ "]";
  Printf.printf "%s" !stringRes;
  Printf.printf "\n";
  Printf.printf "%s\n" expected;
  let testSucc = passTest (!stringRes = expected) in 
  (if testSucc then testsPass := (!testsPass+1));
  testsRun := (!testsRun+1)


let testList = list_cons 2_500 (list_cons 500_000 list_nil)
let testList2 = list_cons 500 (list_cons 1_000 list_nil)
let testList3 = list_cons 12_000 (list_cons 2_500 (list_cons 500_000 list_nil))

let testPrefix =
  Printf.printf "Testing prefix of testList\n";
  test_and_print (prefix testList 4) "[2500,500000,0,0,]"
let testSubtractPositions = 
  Printf.printf "\nTestineg subtract_positions on list 1 and 2\n";
  test_and_print (subtract_positions testList testList2) "[2000,499000,]"

  let testSubtractPositions2 = 
    Printf.printf "\nTestineg subtract_positions on list 1 and 3\n";
    test_and_print (subtract_positions testList testList3) "[-12000,0,0,]"

    let testSubtractPositions3 = 
      Printf.printf "\nTestineg subtract_positions on list 3 and 1\n";
test_and_print (subtract_positions testList3 testList) "[12000,0,0,]"


    let () =
      Printf.printf "All tests finished\n";
      let color = if testsRun = testsPass then "\027[1;4;32m" else "\027[1;4;31m" in 
      Printf.printf "%s%d/%d tests successfull\n" color !testsPass !testsRun;
      if testsPass = testsRun then Printf.printf "All tests passed successfully!\027[0m\n" else Printf.printf "Some tests failed!\027[0m\n"