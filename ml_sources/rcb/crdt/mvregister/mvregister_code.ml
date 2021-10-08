open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: (((operation, value), vector clock), origin), register will only contain ((operation, value), vector clock)  *)

let op_ser = prod_ser string_ser string_ser 

let op_deser = prod_deser string_deser string_deser

let read lock register () = 
  acquire lock;
  let res = list_map (fun x -> snd (fst x)) !register in
  release lock;
  res
  
let effect message register = 
  let rel = (fun m _ -> fst (fst m) = "clear") in 
  let rel01 = (fun m1 m2 -> vect_le (snd m1) (snd m2)) in 
  (* let stabilize = (fun x -> x) in *)
  effectFW rel rel01 rel01 (fst message) register
    
let prepare lock broadcast register value =
    acquire lock; 
    let message = broadcast value in
    effect message register;
    release lock

let apply_thread lock register deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            effect message register;
        | None -> ()
      end;
      release lock;)

let register_init addrs rid = 
    let pair = rcb_init op_ser op_deser addrs rid in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let register = ref (set_empty ()) in
    let lock = newlock () in 
    fork (apply_thread lock register) deliver;
    (read lock register, prepare lock broadcast register)
      