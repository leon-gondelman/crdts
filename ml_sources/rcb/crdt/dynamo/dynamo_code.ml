open Ast
open Rcb_code
open Serialization_code
open Map_code
open List_code
open Vector_clock_code

(* Dynamo-inspired key-value store CRDT. Conflicts are
   resolved by storing all concurrent writes. *)

type 'a kvstore = (string, ('a * vector_clock) alist) amap

let op_ser val_ser = prod_ser string_ser val_ser

let op_deser val_deser = prod_deser string_deser val_deser

let eval = map_lookup

let effect msg kvs =
  let payload = fst (fst msg) in
  let k = fst payload in 
  let v = snd payload in
  let vc = snd (fst msg) in
  let vals =
    match (map_lookup k kvs) with
      Some vs ->
        let is_conc = fun p ->
          let vc' = snd p in
          assert (not (vect_leq vc vc')); (* guaranteed by RCB and our locking *)
          vect_conc vc' vc (* keep all concurrent writes *)
        in
        list_filter is_conc vs
    | None -> list_nil
  in
  map_insert k (list_cons (v, vc) vals) kvs

let read lock kvs k =
  acquire lock;
  let res = eval k !kvs in
  release lock;
  res

let apply_thread lock kvs deliver =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some m -> kvs := effect m !kvs;
        | None -> ();
      end;
      release lock)
      (* Thread.delay 5.0) *)

let upd lock broadcast kvs k v =
  acquire lock;
  (* Since RCB doesn't broadcast to ourselves, we have
     to manually run the update right now. *)
  let msg = broadcast (k, v) in
  kvs := effect msg !kvs;
  release lock

let dynamo_init val_ser val_deser addrs rid =
  let p = rcb_init (op_ser val_ser) (op_deser val_deser) addrs rid in
  let deliver = fst p in
  let broadcast = snd p in
  let kvs = ref (map_empty ()) in
  let lock = newlock () in
  fork (apply_thread lock kvs) deliver;
  (read lock kvs, upd lock broadcast kvs)
