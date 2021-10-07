open Ast
open Rcb_code
open Serialization_code
open List_code
open Set_code

(** TODO: Implement GSet op-based CRDT following 8.1.2 Grow-only Set
    description of the 'Pure Operation-Based Replicated Data Types'
    https://arxiv.org/pdf/1710.04469.pdf paper.
    Try to adapt the code ml_sources/rcb/crdt/counter/counter_code.ml.
*)
type ('a, 'b) sumTy = InjL of 'a | InjR of 'b
type set = string aset


(* Using List serialisation since sets are implemented as lists in aneris *)
let op_ser = string_ser

let op_deser = string_deser

let eval query (set:string aset) = if query = "elems" then (InjL set) else (InjR (set_cardinal set))

let effect message set = 
    let toAdd = (fst (fst message)) in 
    set_add toAdd set

exception NotPossible

let elems lock (set:string aset ref) () =
    acquire lock;
    let res = eval "elems" !set in 
    release lock;
    match res with
    | InjL set -> set
    | _ -> prerr_string "Not possible"; raise NotPossible

let size lock set () =
    acquire lock; 
    let res = eval "size" !set in 
    release lock;
    match res with 
    | InjR size -> size 
    | _ -> prerr_string "Not Possible"; raise NotPossible

let prepare lock broadcast set value =
    acquire lock; 
    let message = broadcast value in
    set := effect message !set;
    release lock

let apply_thread lock set deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            set := effect message !set;
            (* Thread.delay 0.5; *)
        | None -> ()
      end;
      release lock;)


let set_init addrs rid = 
    let pair = rcb_init op_ser op_deser addrs rid in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let set = ref (set_empty ()) in
    let lock = newlock () in 
    fork (apply_thread lock set) deliver;
    (elems lock set, (size lock set, prepare lock broadcast set))
      