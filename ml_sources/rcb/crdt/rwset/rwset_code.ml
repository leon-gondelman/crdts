open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: ((operation, value), vector clock), origin) . *)

let getOP m = fst (fst (fst m))
let getVal m = snd (fst (fst m))
let getVC m = snd (fst m)

(* TODO: can we move this to VC code? *)
let rec vect_eq v1 v2 =
  match v1 with
    Some a1 ->
      (match v2 with
         Some a2 ->
           (fst a1 = fst a2) && vect_eq (snd a1) (snd a2)
       | None -> false)
  | None -> list_is_empty v2

let vect_bottom v =
  match v with
  | Some _ -> false
  | None -> true

(* TODO: can we move this to list code *)  
let rec list_mem_test t l = 
  match list_filter t l with
  | Some _ -> true
  | None -> false 

let stable m s = 
  let testFunc = (fun m' -> getVal m' = getVal m && not (vect_eq (getVC m') (getVC m))) in
  let filteredState = list_filter testFunc s in
  let rmvBasedOnAddCheck = getOP m = "add" && (list_mem_test testFunc s) in
  let rmvBasedOnRmvCheck = getOP m = "rmv" && 
                          ((list_mem_test (fun m' -> getOP m' = "rmv") filteredState) || 
                          not (list_mem_test (fun m' -> getOP m' = "add") filteredState)) in
  let filteredStateBasedOnBottomCheck = list_filter (fun m' -> vect_bottom (getVC m') && 
                                        getOP m' = "rmv" && 
                                        getVal m' = getVal m &&
                                        getOP m = "add" ) s in  
  if rmvBasedOnAddCheck || rmvBasedOnRmvCheck then  list_filter (fun m' -> vect_eq (getVC m') (getVC m)) filteredStateBasedOnBottomCheck
  else filteredStateBasedOnBottomCheck                                       
  
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
  snd (crdt_init addrs rid serialiser ((rel, rel01), rel01) queries stable)