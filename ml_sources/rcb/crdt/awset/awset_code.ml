open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework
open Map_code


  (* Note messages are of the form: (((operation, value), vector clock), origin), 
  the set contains whole messages *)

    let serialiser = {s_ser = prod_ser string_ser string_ser; s_deser = prod_deser string_deser string_deser}

    let rel = (fun m _ ->
      let (((op,_value), _vc),_or) = m in 
      op = "clear" || op = "rmv"
    )
    let rel01 = (fun m1 m2 -> 
      let (((_op1,value1), vc1),or1) = m1 in
      let (((op2,value2), vc2),or2) = m2 in 
      vect_leq_opt vc1 vc2 && op2 = "clear" || value1 = value2
    )
    let stabilize _ s = s
    let read set = 
      let mappet = list_map (fun x -> snd (x)) set in
      let res = ref (set_empty ()) in
      list_iter (fun x -> res := set_add x !res) mappet;
      !res
    let queries = map_insert "read" read (map_empty ())
    let set_init addrs rid = 
      let (queries, prep) = (crdt_init addrs rid serialiser ((rel, rel01), rel01) queries stabilize) in 
      (queries, prep (fun _state payload -> Some payload))