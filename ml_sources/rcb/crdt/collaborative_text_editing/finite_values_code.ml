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

let computeIndex msg list = 
  let length = list_length list in 
  let position = ref (int_of_string (fst (snd msg))) in 
  if !position > length then (Printf.printf "Position greater than length, placing at end\n"; position := length);
  let rightIndex = (if !position = length then None else list_nth list !position) in 
  let leftIndex = (if !position = 0 then None else list_nth list (!position-1)) in 
  let leftIndexSome = match leftIndex with Some x -> float_of_string (fst (snd (fst x))) | None -> 0.0 in 
  let rightIndexSome = match rightIndex with Some x -> float_of_string (fst (snd (fst x))) | None -> 1.0 in 
  let index: float = Float.div (Float.add rightIndexSome leftIndexSome) 2.0 in  (*Are you absolutely bunkers, OCaml? I can't just use + and / with floats?!*)
Printf.printf "Index is %f\n" index;
  string_of_float index

  




let list_sort f l =
  let res = ref list_nil in
  list_iter (place_in_list f res) l;
  !res
let known_queries =
  map_insert
    "read"
    (fun pset -> list_iter (fun m -> Printf.printf "(%s,%s)" (fst (snd m)) (snd (snd m))) 
      (list_sort (fun m1 m2 -> (float_of_string (fst (snd m1)) < float_of_string (fst (snd m2)))) pset)) 
    (map_empty ())

let stabilize _m s = s

let serializer = {s_ser = prod_ser string_ser (prod_ser string_ser string_ser); 
                  s_deser = prod_deser string_deser (prod_deser string_deser string_deser)}

let editor_init addrs rid =
  crdt_init addrs rid serializer ((rel, rel0), rel1) known_queries stabilize