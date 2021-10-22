open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: (((operation, (position, value)), vector clock), origin), 
  the set contains whole messages *)

let getOp (m : 'value msg) = fst (fst (fst m))
let getPos (m : 'value msg) = fst (snd (fst (fst m)))
let getVal (m : 'value msg) = snd (snd (fst (fst m)))
let getVC (m : 'value msg) = snd (fst m)
let getOr (m : 'value msg) = snd m 

let rel (m1 : 'value msg) (s : 'value msg aset) =
  let rec concurrentAndLoss set = 
    match list_head set with
    | Some m2 ->
      (vect_conc (getVC m2) (getVC m1) && getOr m2 < getOr m1)
      || concurrentAndLoss (list_tail set)
    | None -> false 
  in
  getOp m1 = "delete" || concurrentAndLoss s

let rel0 (m1 : 'value msg) (m2 : 'value msg) = 
  getOp m2 = "delete" && (getPos m1) = (getPos m2) && vect_leq (getVC m1) (getVC m2)
  
let rel1 (m1 : 'value msg) (m2 : 'value msg) =
  vect_conc (getVC m1) (getVC m2) && getOr m2 < getOr m1

let place_in_list comparator inputList_ref toInsert = 
  let rec inner comparator inputList_ref toInsert = match list_head (inputList_ref) with
    | Some lValue -> ( match list_head (list_tail (inputList_ref)) with
                | Some rValue -> if (comparator toInsert lValue) then Some (toInsert, list_cons lValue (list_tail (inputList_ref)))
                            else if (comparator toInsert rValue) then Some (lValue, list_cons toInsert (list_tail (inputList_ref)))
                            else Some (lValue, list_cons rValue (inner comparator (list_tail (list_tail (inputList_ref))) toInsert))
                | None -> 
                          if (comparator toInsert lValue) then Some (toInsert, list_cons lValue None) 
                          else Some (lValue, list_cons toInsert None) 
                )              
    | None -> Some (toInsert, None)                
  in                        
  inputList_ref := inner comparator !inputList_ref toInsert 

let list_sort comparator inputList =
  let res = ref list_nil in
  list_iter (place_in_list comparator res) inputList;
  !res

let rec findMinInList inputList minSoFar = 
  let minSoFarVal = float_of_string (fst (snd minSoFar)) in
  match list_head inputList with 
  | Some lValueMessage -> 
    (
      let lValue = float_of_string (fst (snd lValueMessage)) in 
      match list_head (list_tail inputList) with
      | Some rValueMessage -> 
        (
          let rValue = float_of_string (fst (snd rValueMessage)) in 
          if lValue < minSoFarVal then findMinInList (list_tail inputList) lValueMessage
          else if rValue < minSoFarVal then findMinInList (list_tail inputList) rValueMessage
          else findMinInList (list_tail inputList) minSoFar
        )
      | None -> 
        (
          if lValue < minSoFarVal then findMinInList (list_tail inputList) lValueMessage
          else findMinInList (list_tail inputList) minSoFar
        )
      )
  | None -> 
    (
      minSoFar
    )
  


let list_sort2 (inputList: ((string * (string * string) * vector_clock * int) alist)) = 
  let result = ref list_nil in 
  let length = list_length inputList in 
  let i = ref 0 in 
  let lastInsert = ref ((("", ("0", "")), vect_make 1 0), ("", 0)) in
  while (!i < length) do 
    let insert = findMinInList inputList !lastInsert in 
    result := list_cons (insert) inputList; 
    i := !i+1;
    lastInsert := insert
  done;
  !result


let known_queries =
  map_insert
    "read"
    (fun pset -> list_iter (fun m -> Printf.printf "(%s,%s)" (fst (snd m)) (snd (snd m))) 
      (*(list_sort (fun m1 m2 -> (float_of_string (fst (snd m1)) < float_of_string (fst (snd m2)))) pset)) *)
      (list_sort2 pset))
    (map_empty ())

let stabilize _m s = s

let serializer = {s_ser = prod_ser string_ser (prod_ser string_ser string_ser); 
                  s_deser = prod_deser string_deser (prod_deser string_deser string_deser)}

let editor_init addrs rid =
  crdt_init addrs rid serializer ((rel, rel0), rel1) known_queries stabilize