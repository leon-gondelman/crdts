open Ast
open List_code
open Set_code
open Map_code
open Vector_clock_code
open Network_util_code
open Rcb_stabilise
open Serialization_code

type 'value payload = (string * 'value)
type ('a, 'b) known_queries = (string, 'a payload aset -> 'b) amap
type 'value msg = ((('value payload) * vector_clock) * int)

let effectFW rel rel0 rel1 mes stateRef =
  let relMesStateBool = rel mes !stateRef in
  let newSet = list_filter (fun x -> ((not (rel0 x mes)) && relMesStateBool)
  || ((not (rel1 x mes)) && (not(relMesStateBool)))) !stateRef in
  if not (relMesStateBool) then stateRef := (set_add mes newSet) else stateRef := newSet


let apply_thread lock stateRef deliver effect () =
  loop_forever (fun () ->
      acquire lock;
      begin
        match (deliver ()) with
          Some message ->
            effect message stateRef;
        | None -> ()
      end;
      release lock;)

let prepare lock broadcast stateRef effect payload =
    acquire lock;
    let message = broadcast payload in
    effect message stateRef;
    release lock

let read
    (lock : Mutex.t)
    (stateRef : ('value msg alist) ref)
    (knw_queries : ('a, 'res) known_queries) (query_name : string) =
  let query = (map_lookup query_name knw_queries) in
  match query with
  | Some f ->
    acquire lock;
    let payload_set = list_map (fun x -> fst (fst x)) !stateRef in
    let res = Some (f payload_set) in
    release lock;
    res
  | None -> None

let crdt_init
    (addrs : saddr alist)
    (rid : int)
    (payload_serializer : 'value payload serializer)
    rels
    (known_queries : ('value, 'res) known_queries)
    (stabilize : 'value msg -> 'value msg aset -> 'value msg aset) =
  let ((rel, rel0), rel1) = rels in
  let stateRef = ref (set_empty ()) in
  let pair =
    rcb_init
      payload_serializer.s_ser
      payload_serializer.s_deser
      addrs rid stateRef stabilize in
  let (deliver, broadcast) = pair in
  let lock = newlock () in
  let effect = effectFW rel rel0 rel1 in
  fork (apply_thread lock stateRef deliver effect) ();
  (read lock stateRef known_queries, prepare lock broadcast stateRef effect)
