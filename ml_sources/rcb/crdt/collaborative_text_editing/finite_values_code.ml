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
  let concurrentAndLoss set = 
    match list_head set with
    | Some m2 ->
      (vect_conc (getVC m2) (getVC m1) && getOr m2 < getOr m1)
      || concurrentAndLoss (list_tail s)
    | None -> false 
  in
  getOp m1 = "Delete" || concurrentAndLossTest s

let rel0 (m1 : 'value msg) (m2 : 'value msg) =  
  vect_conc (getVC m1) (getVC m2) && getOr m2 < getOr m1

let rel1 (m1 : 'value msg) (m2 : 'value msg) =
  getOp m1 = "Delete" && getPos m1 = getPos m2 && vect_leq (getVC m2 m1)

let rec place_in_list f l_ref m = 
  let rec inner f l_ref m = match !l_ref with
  | x::y::t -> 
    if (f m x) then m::x::y::t
    else if (f m y) then x::m::y::t
    else x::y::(inner f (ref t) m)    
  | x::[ ] -> if (f m x) then m::x::[ ] else x::m::[ ]
  | [ ] -> [ m ]
  in
  l_ref := inner f l_ref m 

let list_sort f l =
  let res = ref list_nil in
  lister_iter (place_in_list f res) l;
  !res
let known_queries =
  map_insert
    "read"
    (fun pset -> list_map (getVal) (list_sort (fun m1 m2 -> (getPos m1 < getPos m2)) pset)) 
    (map_empty ())

let stabilize m s = 
  list_filter (fun x -> not (vect_conc (getVC x) (getVC m) && getOr m < getOr x)) s

let register_init addrs rid (serializer : 'value payload serializer) =
  crdt_init addrs rid serializer ((rel, rel01), rel01) known_queries stabilize