open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

    let read lock set () = 
      acquire lock;
      let rmvSet = list_filter (fun x -> (fst (fst (fst x))) = "rmv") !set in
      let rmvSetClean = list_map (fun x -> snd (fst (fst x))) rmvSet in
      let addSet = list_filter (fun x -> (fst (fst (fst x))) = "add") !set in
      let addSetClean = list_map (fun x -> snd (fst (fst x))) addSet in 
      let res = list_filter (fun x -> not (list_mem x rmvSetClean)) addSetClean in 
      release lock;
      res

      (*TODO Fill out stable function*)
    let stable = fun _ s -> s

    let read messages = 
      let rmvSet = list_filter (fun x -> fst x = "rmv") messages in 
      let rmvSetClean = list_map (fun x -> snd x) rmvSet in 
      let addSet = list_filter (fun x -> fst x = "add") messages in 
      let addSetClean = list_map (fun x -> snd x) addSet in 
      list_filter (fun x -> not (list_mem x rmvSetClean)) addSetClean

    let queries = map_insert "read" read (map_empty ())

    let rel = (fun m _ -> fst (fst (fst m)) = "clear")
    let rel01 = (fun m1 m2 -> (vect_leq (snd (fst m1)) (snd (fst m2)) && ((fst (fst (fst m2)) = "clear") || (snd (fst (fst m1)) = snd (fst (fst m2))))))  
    let serialiser = {s_ser = prod_ser string_ser string_ser; s_deser = prod_deser string_deser string_deser}

    let set_init addrs rid = 
      crdt_init addrs rid serialiser ((rel, rel01), rel01) queries stable