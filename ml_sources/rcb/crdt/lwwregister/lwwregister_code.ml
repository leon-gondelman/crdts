open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

let rel (m1 : 'value msg) (s : 'value msg aset) =
  match list_head s with
  | Some m2 ->
    (fst (fst (fst m1)) = "clear")
    || ((vect_conc (snd (fst m1)) (snd (fst m2))) && (snd m1 >= snd m2))
  | None -> false

let rel01 (m1 : 'value msg) (m2 : 'value msg)  =
  (vect_leq (snd (fst m1)) (snd (fst m2)))
  || (vect_conc (snd (fst m1)) (snd (fst m2)) && snd m1 >= snd m2)

let known_queries: (string, string aset) known_queries =
  map_insert
    "read"
    (fun pset -> list_map (snd) pset) (map_empty ())

let stabilize _m s = s

let serializer = {s_ser = prod_ser string_ser string_ser; s_deser = prod_deser string_deser string_deser}

let register_init addrs rid =
  snd (crdt_init addrs rid serializer ((rel, rel01), rel01) known_queries stabilize)
