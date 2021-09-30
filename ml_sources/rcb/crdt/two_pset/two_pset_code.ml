open Ast
open Rcb_code
open Serialization_code
open List_code
open Set_code

(** TODO: Implement of 2PSet op-based CRDT following 8.1.3 Two-Phase Set
    description of the 'Pure Operation-Based Replicated Data Types'
    https://arxiv.org/pdf/1710.04469.pdf paper.
    Try to adapt the code ml_sources/rcb/crdt/counter/counter_code.ml. *)

type ('a, 'b) sumTy = InjL of 'a | InjR of 'b
type set = string aset


(* Using List serialisation since sets are implemented as lists in aneris *)
let op_ser = prod_ser string_ser string_ser 

let op_deser = prod_deser string_deser string_deser

let eval query (set:string aset) = if query = "elems" then (InjL set) else (InjR (set_cardinal set))

let effect message addSet removeSet = 
    let op = fst (fst (fst message)) in 
    let value = snd (fst (fst message)) in 
    if op = "add" && not (set_mem value removeSet) then (
    ( removeSet, set_add value addSet ))
    else if op = "rmv" then (
        (set_add value removeSet, list_filter (fun x -> x <> value) addSet)
    )
    else (removeSet, addSet)

    
let elems lock (set:string aset ref) () =
    acquire lock;
    let res = eval "elems" !set in 
    release lock;
    match res with
    | InjL set -> set
    | _ -> prerr_string "Not possible"; exit 2 

let size lock set () =
    acquire lock; 
    let res = eval "size" !set in 
    release lock;
    match res with 
    | InjR size -> size 
    | _ -> prerr_string "Not Possible"; exit 2

let prepare lock broadcast addSet removeSet valuePair =
    acquire lock; 
    let message = broadcast valuePair in
    let removeSetValues, addSetValues = effect message !addSet !removeSet in 
    removeSet := removeSetValues;
    addSet := addSetValues;
    release lock

let apply_thread lock addSet removeSet deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            let removeSetValues, addSetValues = effect message !addSet !removeSet in 
            removeSet := removeSetValues;
            addSet := addSetValues;
            (* Thread.delay 0.5; *)
        | None -> ()
      end;
      release lock;)


let set_init addrs rid = 
    let pair = rcb_init op_ser op_deser addrs rid in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let addSet = ref (set_empty ()) in
    let removeSet = ref (set_empty ()) in
    let lock = newlock () in 
    fork (apply_thread lock addSet removeSet) deliver;
    (elems lock addSet, (size lock addSet, prepare lock broadcast addSet removeSet))