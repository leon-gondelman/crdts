open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: (((operation, value), vector clock), origin), set will only contain ((operation, value), vector clock)  *)

(* type register = string aset *)

let op_ser = prod_ser string_ser string_ser 

let op_deser = prod_deser string_deser string_deser

let read lock (register:string aset ref) () = 
  acquire lock;
  let res = list_map (fun x -> snd (fst x)) !register in
  release lock;
  res
  
let vect_le v1 v2 = not (vect_leq v2 v1)

let effect message (register:string aset ref) = 
  let rel = (fun m s -> if fst (fst m) = "clear" then true else false) in 
  let rel01 = (fun m1 m2 -> vect_le (snd m1) (snd m2)) in 
  let stabilize = (fun x -> x) in 
  effectFW rel rel01 rel01 (fst message) register
    
let prepare lock broadcast (register:string aset ref) value =
    acquire lock; 
    let message = broadcast value in
    effect message register;
    release lock

let apply_thread lock (register:string aset ref) deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            effect message register;
        | None -> ()
      end;
      release lock;)

let set_init addrs rid = 
    let pair = rcb_init op_ser op_deser addrs rid in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let (register:string aset ref) = ref (set_empty ()) in
    let lock = newlock () in 
    fork (apply_thread lock register) deliver;
    (read lock register, prepare lock broadcast register)
      