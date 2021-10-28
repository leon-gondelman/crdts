open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Map_code
open Pure_op_based_framework

  let rel = (fun m _ -> fst (fst (fst m)) = "clear")
  let rel01 = (fun m1 m2 -> vect_leq_opt (snd (fst m1)) (snd (fst m2)))
  let stabilize = (fun _ x -> x)

let op_ser = prod_ser string_ser string_ser 
let op_deser = prod_deser string_deser string_deser
let readFunc register = list_map (fun x -> snd ( fst (fst x))) !register
let ser = {s_ser = op_ser; s_deser = op_deser}
let queries: (string, string aset) known_queries = map_insert "read" (fun m -> list_map (fun x -> snd x) m) (map_empty ())
let register_init addr rid = 
    let (queries, prepare) = (crdt_init addr rid ser ((rel, rel01), rel01) queries stabilize) in
    (queries, prepare (fun _state payload -> payload))
  