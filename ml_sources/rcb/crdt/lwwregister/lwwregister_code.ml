open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: ((( operation, value), vector clock), origin), 
  the set contains whole messages *)

let rel (m1 : 'value msg) (s : 'value msg aset) =
  match list_head s with
  | Some m2 ->
    let (((op1,_value1), vc1), or1) = m1 in 
    let (((_op2, _value2), vc2), or2) = m2 in
    op1 = "clear" || (vect_conc_opt vc1 vc2) && (or1 >= or2)
  | None -> false

let rel01 (m1 : 'value msg) (m2 : 'value msg)  =
let (((_op1,_value1), vc1), or1) = m1 in 
let (((_op2, _value2), vc2), or2) = m2 in
vect_leq_opt vc1 vc2 || vect_conc_opt vc1 vc2 && or1 >= or2

let known_queries: (string, string aset) known_queries =
  map_insert
    "read"
    (fun pset -> list_map (snd) pset) (map_empty ())

let stabilize _m s = s

let serializer = {s_ser = prod_ser string_ser string_ser; s_deser = prod_deser string_deser string_deser}

let register_init addrs rid =
  let (queries, prepare) = (crdt_init addrs rid serializer ((rel, rel01), rel01) known_queries stabilize) in 
  (queries, prepare (fun _state payload -> Some payload))
