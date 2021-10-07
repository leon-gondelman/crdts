open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Set_code
open Vector_clock_code

(** Last Write Wins Register 
    Based on 
    https://hal.inria.fr/inria-00555588/document.
*)
type registerVal = string (* Strings can sort of hold anything so for greatest flexibility a string is ued. Further serialisation/deserialisation can be done on the string to convert to any other data type. *)

let op_ser = string_ser

let op_deser = string_deser

let eval (register:registerVal) () = register

let effect message (register:registerVal) our_ts our_id = 
    let ((newRegVal, their_vc), orig) = message in 
    let their_ts = vect_nth their_vc orig in 
    if their_ts > our_ts then newRegVal else
      if their_ts = our_ts then (if orig <= our_id then newRegVal else register) (* <= is important since the msg may be from ourself* *)
      else register

let prepare lock broadcast (register:registerVal ref) value =
    acquire lock; 
    let message = broadcast value in
    let our_vc = snd (fst message) in
    let our_id = snd message in 
    let our_ts = vect_nth our_vc our_id in 
    register := effect message !register our_ts our_id;
    release lock

let apply_thread lock register our_ts our_id deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            register := effect message !register our_ts our_id;
            (* Thread.delay 0.5; *)
        | None -> ()
      end;
      release lock;)


let register_init addrs rid = 
    let pair = rcb_init op_ser op_deser addrs rid in 
    let deliver = fst pair in
    let broadcast = snd pair in
    let register = ref ("") in
    let lock = newlock () in 
    fork (apply_thread lock register 0 rid) deliver;
    (eval !register, prepare lock broadcast register)
      