open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: (((operation, value), vector clock), origin), register contains (((operation, value), vector clock), origin)  *)

let op_ser = prod_ser string_ser string_ser 

let op_deser = prod_deser string_deser string_deser

let read lock register () = 
  acquire lock;
  let res = list_map (fun x -> snd ( fst (fst x))) !register in
  release lock;
  res
  
let effect message register = 
  let rel = (fun m _ -> fst (fst (fst m)) = "clear") in 
  let rel01 = (fun m1 m2 -> vect_leq (snd (fst m1)) (snd (fst m2))) in 
  (* let stabilize = (fun x -> x) in *)
  effectFW rel rel01 rel01 (message) register
    
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
    let register = ref (set_empty ()) in
    let pair = rcb_init op_ser op_deser addrs rid register (fun _ reg -> reg) in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let lock = newlock () in 
    fork (apply_thread lock register) deliver;
    (read lock register, prepare lock broadcast register)
      