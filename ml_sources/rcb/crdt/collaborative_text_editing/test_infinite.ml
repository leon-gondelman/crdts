open Infinite_values_code
open List_code

let testsRun = ref 0
let testsPass = ref 0

let passTest check = 
  if check then Printf.printf "\027[1;32mTEST SUCCESS\027[0m\n-----------------------" else Printf.printf "\027[1;31mTEST FAILED\027[0m\n-----------------------"; check

let test_and_print result expected = 
  let stringRes = ref "[" in 
  list_iter (fun x -> stringRes := !stringRes ^ (Printf.sprintf "%d," x)) result;
  stringRes := !stringRes ^ "]";
  Printf.printf "Result = %s\n" !stringRes;
  Printf.printf "Expect = %s\n" expected;
  let testSucc = passTest (!stringRes = expected) in 
  (if testSucc then testsPass := (!testsPass+1));
  testsRun := (!testsRun+1)


let testList = list_cons 2_500 (list_cons 500_000 list_nil)
let testList2 = list_cons 500 (list_cons 1_000 list_nil)
let testList3 = list_cons 12_000 (list_cons 2_500 (list_cons 500_000 list_nil))
let testList4 = list_cons 42 (list_cons 12_000 (list_cons 2_500 (list_cons 500_000 list_nil)))

let testLePosition = 
  let result = le_positions testList2 testList in 
  let resultList = if result then (list_cons 1 list_nil) else (list_cons 0 list_nil) in
  Printf.printf "\nTesting le_position on 2 and 1\n";
  test_and_print resultList "[1,]"

  let testLePosition2 = 
    let result = le_positions testList3 testList2 in 
    let resultList = if result then (list_cons 1 list_nil) else (list_cons 0 list_nil) in
    Printf.printf "\nTesting le_position on 3 and 2\n";
    test_and_print resultList "[0,]"

 let testMinPos = 
  let result = min_positions testList2 testList3 in 
  Printf.printf "\nTesting Min Position on 2 and 3\n";
  test_and_print result "[500,1000,]"

let testMinPos2 = 
  let result = min_positions testList3 testList2 in 
  Printf.printf "\nTesting Min Position on 3 and 2\n";
  test_and_print result "[500,1000,]"
  
let testLePosition3 = 
  let result = le_positions testList2 testList3 in 
  let resultList = if result then (list_cons 1 list_nil) else (list_cons 0 list_nil) in
  Printf.printf "\nTesting le_position on 2 and 3\n";
  test_and_print resultList "[1,]"

let testLePositionEq = 
  let result = le_positions testList3 testList3 in 
  let resultList = if result then (list_cons 1 list_nil) else (list_cons 0 list_nil) in
  Printf.printf "\nTesting le_position on 3 and 3\n";
  test_and_print resultList "[0,]"
  
let testPrefix =
  Printf.printf "Testing prefix of testList\n\n";
  test_and_print (prefix testList 4) "[2500,500000,0,0,]"

let testPadding = 
  let (res1, res2) = padListWithPrependedZero testList testList4 in 
  Printf.printf "\nTesting prepending 0s to pad list lengths\n"; 
  test_and_print res1 "[0,0,2500,500000,]";
  Printf.printf "\nTesting prepending 0s to pad list lengths 2\n";
  test_and_print res2 "[42,12000,2500,500000,]"

let testListBase10_1 = list_cons 2 (list_cons 0 list_nil)
let testListBase10_2 = list_cons 1 (list_cons 1 list_nil)

let testSubtractPositions = 
  Printf.printf "\nTestineg subtract_positions on list 1 and 2\n\n";
  test_and_print (subtract_positions testListBase10_1 testListBase10_2 10) "[0,9,]"

let testSubtractPositions2 = 
  Printf.printf "\nTestineg subtract_positions on list 2 and 2\n\n";
  test_and_print (subtract_positions testListBase10_2 testListBase10_2 10) "[0,-9,]"

let () =
  Printf.printf "\n\027[1mAll tests finished\n";
  let color = if testsRun = testsPass then "\027[1;4;32m" else "\027[1;4;31m" in 
  Printf.printf "%s%d/%d tests successfull\n" color !testsPass !testsRun;
  if testsPass = testsRun then Printf.printf "All tests passed successfully!\027[0m\n" else Printf.printf "Some tests failed!\027[0m\n"