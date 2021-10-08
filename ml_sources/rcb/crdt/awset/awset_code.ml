open Ast
open Rcb_minimal_code
open Serialization_code
open List_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework

(* Note messages are of the form: (((operation, value), vector clock), origin), set will only contain ((operation, value), vector clock)  *)

let op_ser = prod_ser string_ser string_ser 

let op_deser = prod_deser string_deser string_deser

let read lock set () = 
  acquire lock;
  let res = list_map (fun x -> snd (fst x)) !set in
  release lock;
  res
  
let effect message set = 
  let rel = (fun m _ -> fst (fst m) = "clear" || fst (fst m) = "rmv") in 
  let rel01 = (fun m1 m2 -> 
    Printf.printf "Testing: %s\n" (string_of_bool (vect_le (snd m1) (snd m2)));
    (vect_le (snd m1) (snd m2)) && ((fst (fst m2) = "clear") || (snd (fst m1) = snd (fst m2)))) in 
  (* let stabilize = (fun x -> x) in *)
  effectFW rel rel01 rel01 (fst message) set
    
let prepare lock broadcast set value =
    acquire lock; 
    let message = broadcast value in
    effect message set;
    release lock

let apply_thread lock set deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            effect message set;
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
    (read lock set, prepare lock broadcast set)
      