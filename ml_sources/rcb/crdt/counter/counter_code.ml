open Ast
open Rcb_code
open Serialization_code

(* Note messages are of the form: (( value, vector clock), origin), 
  the set contains whole messages *)

type counter = int

let op_ser = int_ser

let op_deser = int_deser

let eval counter = counter

let effect msg counter =
  let delta = (fst (fst msg)) in
  counter + delta

let read lock counter () =
  acquire lock;
  let res = eval !counter in
  release lock;
  res

let apply_thread lock counter deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some m ->
            counter := effect m !counter;
            (* Thread.delay 0.5; *)
        | None -> ()
      end;
      release lock;)

let upd lock broadcast counter delta =
  acquire lock;
  (* Since RCB doesn't broadcast to ourselves, we have
     to manually run the update right now. *)
  let msg = broadcast delta in
  counter := effect msg !counter;
  release lock

let counter_init addrs rid =
  let p = rcb_init op_ser op_deser addrs rid in
  let deliver = fst p in
  let broadcast = snd p in
  let counter = ref 0 in
  let lock = newlock () in
  fork (apply_thread lock counter) deliver;
  ( read lock counter,upd lock broadcast counter)
