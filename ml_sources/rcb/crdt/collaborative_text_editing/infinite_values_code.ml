open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework
open Network_util_code
open Collaborative_editor_shared

(* Note messages are of the form: (((operation, (position alist, value)), vector clock), origin), 
  the set contains whole messages *)

  let base = 2*10_00000


let comparator m1 m2 = 
  let posList1 = getPosFromPayload m1 in 
  let posList2 = getPosFromPayload m2 in 
  let rec comparatorInner list1 list2 = 
    if list_head list1 = list_head list2 then comparatorInner (list_tail list1) (list_tail list2)
    else int_of_string (unSOME (list_head list1)) < int_of_string (unSOME (list_head list2))) in 
  comparatorInner posList1 posList2

let comparator' m1 m2 =  
  let posList1 = getPos m1 in 
  let posList2 = getPos m2 in 
  let rec comparatorInner list1 list2 = 
    if list_head list1 = list_head list2 then comparatorInner (list_tail list1) (list_tail list2)
    else int_of_string (unSOME (list_head list1)) < int_of_string (unSOME (list_head list2))) in 
  comparatorInner posList1 posList2




let rel (m1 : 'value msg) (s : 'value msg aset) =
  let rec concurrentAndLoss set = 
    match list_head set with
    | Some m2 ->
      (vect_conc_opt (getVC m2) (getVC m1) && getOr m2 < getOr m1)
      || concurrentAndLoss (list_tail set)
    | None -> false 
  in
  getOp m1 = "delete" || concurrentAndLoss s

let rel0 (m1 : 'value msg) (m2 : 'value msg) = 
  getOp m2 = "delete" && (getPos m1) = (getPos m2) && vect_leq_opt (getVC m1) (getVC m2)
  
let rel1 (m1 : 'value msg) (m2 : 'value msg) =
  vect_conc_opt (getVC m1) (getVC m2) && getOr m2 < getOr m1  

  
  let compute_position state index = 
    let listLength = list_length state in
    if listLength = 0 then base/2
    else (
      let indexInt = int_deser index in
      let sortedState = list_sort comparator' state in  
      let elementPre = list_nth sortedState (indexInt-1) in
      let elementSuc = list_nth sortedState indexInt in
      (* elementSucPos and elementPrePos computations have to wait, 
      since we need to make sure we don't get assertion fails with unSOME *)
      if indexInt = 0 then (
        (*Change to check each level of the array*)
        let elementSucPos =  int_of_string (getPos (unSOME elementSuc)) in 
        string_of_float (elementSucPos / 2)
      )
      else if indexInt = list_length sortedState then 
        (*Change to consider every level of the array*) 
        let elementPrePos = int_of_string (getPos (unSOME elementPre)) in
        string_of_float ((elementPrePos + 1) / 2)
      else (
        (*Change to check each level of the array*)
        let elementPrePos = int_of_string (getPos (unSOME elementPre)) in
        let elementSucPos =  int_of_string (getPos (unSOME elementSuc)) in 
        string_of_float ((elementPrePos + elementSucPos) / 2)  
      )  
    )  
  
  let get_position state index = 
    let indexInt = int_deser index in 
    let sortedState = list_sort comparator' state in
    getPos (unSOME (list_nth sortedState indexInt))
    

(* Following replaced by the function just below for now. Are they equal?
  let list_to_string (list: (string alist)) : string = 
    let start = "[" in 
    let mid = list_fold (^) start list in 
    mid ^ "]"
 *)
  let list_to_string (list: string alist): string = 
    (list_ser string_ser list)

    
  let known_queries =
    map_insert
      "read"
      (fun pset -> list_iter (fun m -> Printf.printf "(%s,%s)" (list_to_string (fst (snd m))) (snd (snd m))) 
        (list_sort comparator pset)) 
      (map_empty ())
  
      let transformPayload state payload =
        let (operation, (index, value)) = payload in 
        if(not (is_valid_index state index)) then (None) else 
        if(fst payload = "write") then 
          (
            let newPosition = compute_position  state index in 
            Some (operation, (newPosition, value))
          )
        else
          (
            let newPos = get_position state index in 
            Some (operation, (newPos, value))
          )

      let serializer = {s_ser = prod_ser string_ser (prod_ser (list_ser string_ser) string_ser); 
        s_deser = prod_deser string_deser (prod_deser (list_deser string_deser) string_deser)}

      
      let editor_init addrs rid =  
        let (query, prepare) = crdt_init addrs rid serializer ((rel, rel0), rel1) known_queries stabilize in 
        (query, prepare (transformPayload))