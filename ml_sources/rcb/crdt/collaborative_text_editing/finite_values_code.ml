open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework
open Network_util_code


(* Note messages are of the form: (((operation, (position, value)), vector clock), origin), 
  the set contains whole messages *)

let getOp (m : 'value msg) = fst (fst (fst m))
let getPos (m : 'value msg) = fst (snd (fst (fst m)))
let getVal (m : 'value msg) = snd (snd (fst (fst m)))
let getVC (m : 'value msg) = snd (fst m)
let getOr (m : 'value msg) = snd m 
let getPosFromPayload p = fst (snd p) 
let comparator m1 m2 = float_of_string (getPosFromPayload m1) < float_of_string (getPosFromPayload m2)
let comparator' m1 m2 = float_of_string (getPos m1) < float_of_string (getPos m2)

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

let place_in_list f l_ref m = 
  let rec inner f l m = match list_head (l) with
    | Some x -> ( match list_head (list_tail (l)) with
                | Some y -> if (f m x) then Some (m, list_cons x (list_tail (l)))
                            else if (f m y) then Some (x, list_cons m (list_tail (l)))
                            else Some (x, list_cons y (inner f (list_tail (list_tail (l))) m))
                | None -> 
                          if (f m x) then Some (m, list_cons x None) 
                          else Some (x, list_cons m None) 
                )              
    | None -> Some (m, None)                
  in                        
  l_ref := inner f !l_ref m 

let list_sort f l =
  let res = ref list_nil in
  list_iter (place_in_list f res) l;
  !res

let compute_position state index = 
  let listLength = list_length state in
  if listLength = 0 then "0.5"  
  else (
    let indexInt = int_deser index in
    let sortedState = list_sort comparator' state in  
    let elementPre = list_nth sortedState (indexInt-1) in
    let elementSuc = list_nth sortedState indexInt in
    (* elementSucPos and elementPrePos computations have to wait, 
    since we need to make sure we don't get assertion fails with unSOME *)
    if indexInt = 0 then (
      let elementSucPos =  float_of_string (getPos (unSOME elementSuc)) in 
      string_of_float (Float.div elementSucPos  2.0)
    )
    else if indexInt = list_length sortedState then  
      let elementPrePos = float_of_string (getPos (unSOME elementPre)) in
      string_of_float (Float.div (Float.add elementPrePos 1.0) 2.0)
    else (
      let elementPrePos = float_of_string (getPos (unSOME elementPre)) in
      let elementSucPos =  float_of_string (getPos (unSOME elementSuc)) in 
      string_of_float (Float.div (Float.add elementPrePos elementSucPos) 2.0)  
    )  
  )  

let get_position state index = 
  let indexInt = int_deser index in 
  let sortedState = list_sort comparator' state in
  getPos (unSOME (list_nth sortedState indexInt))
  
let is_valid_index state index = 
  let indexInt = int_deser index in
  if list_length state = 0 && indexInt = 0 then true
  else if (indexInt < 0) || (indexInt > (list_length state)) then false
  else true  
  
let known_queries =
  map_insert
    "read"
    (fun pset -> list_iter (fun m -> Printf.printf "(%s,%s)" (fst (snd m)) (snd (snd m))) 
      (list_sort comparator pset)) 
    (map_empty ())

let stabilize _m s = s

let serializer = {s_ser = prod_ser string_ser (prod_ser string_ser string_ser); 
                  s_deser = prod_deser string_deser (prod_deser string_deser string_deser)}


let transformPayload state payload =
  let (operation, (index, value)) = payload in 
  if(fst payload = "write") then 
    (
      let newPosition = compute_position  state index in 
      (operation, (newPosition, value))
    )
  else
    (
      let newPos = get_position state index in 
      (operation, (newPos, value))
    )

let editor_init addrs rid =  
  let (query, prepare) = crdt_init addrs rid serializer ((rel, rel0), rel1) known_queries stabilize in 
  (query, prepare (transformPayload))