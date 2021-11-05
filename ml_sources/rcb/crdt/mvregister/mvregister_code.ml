open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Map_code
open Pure_op_based_framework

(* Note messages are of the form: ((( operation, value), vector clock), origin), 
  the set contains whole messages *)
let rel = (fun m _ ->
  let (((op,_value), _vc), _or) = m in 
  op = "clear")
let rel01 = (fun m1 m2 ->
  let (((_op1,_value1), vc1), _or1) = m1 in 
  let (((_op2, _value2), vc2), _or2) = m2 in
  vect_leq_opt vc1 vc2
)
let stabilize = (fun _ x -> x)

let op_ser = prod_ser string_ser string_ser 
let op_deser = prod_deser string_deser string_deser
let readFunc register = list_map (fun x ->
  let (((_op,value), _vc), _or) = x in 
  value !register)
let ser = {s_ser = op_ser; s_deser = op_deser}
let queries: (string, string aset) known_queries = map_insert "read" (fun m -> list_map (fun x -> snd x) m) (map_empty ())
let register_init addr rid = 
    let (queries, prepare) = (crdt_init addr rid ser ((rel, rel01), rel01) queries stabilize) in
    (queries, prepare (fun _state payload -> Some payload))
  