open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework
open Network_util_code

let getOp (m : 'value msg) = fst (fst (fst m))
let getPos (m : 'value msg) = fst (snd (fst (fst m)))
let getVal (m : 'value msg) = snd (snd (fst (fst m)))
let getVC (m : 'value msg) = snd (fst m)
let getOr (m : 'value msg) = snd m 
let getPosFromPayload p = fst (snd p) 

let is_valid_index state index = 
  let indexInt = int_deser index in
  if list_length state = 0 && indexInt = 0 then true
  else if (indexInt < 0) || (indexInt > (list_length state)) then false
  else true  

let stabilize _m s = s


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
    