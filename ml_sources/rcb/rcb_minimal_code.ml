open Ast
open List_code
open Queue_code
open Network_util_code
open Vector_clock_code
open Serialization_code

let rep_id_ser = int_ser

let rep_id_deser = int_deser

let seqnum_ser = int_ser

let seqnum_deser = int_deser

let ack_msg_ser = prod_ser seqnum_ser rep_id_ser

let ack_msg_deser = prod_deser seqnum_deser rep_id_deser

let broadcast_msg_ser (val_ser[@metavar]) =
  prod_ser (prod_ser val_ser vect_serialize) rep_id_ser
let broadcast_msg_deser (val_deser[@metavar]) =
    prod_deser (prod_deser val_deser vect_deserialize) rep_id_deser

let msg_ser (val_ser[@metavar]) =
  sum_ser ack_msg_ser (broadcast_msg_ser val_ser)

let msg_deser (val_deser[@metavar]) =
  sum_deser ack_msg_deser (broadcast_msg_deser val_deser)

let rec loop_forever thunk =
  thunk ();
  loop_forever thunk

(* Pops the top of the queue while the predicate holds of it. *)
let rec take_while pred q =
  match (queue_peek_opt q) with
    Some e ->
      if (pred e) then
        let p = unSOME (queue_take_opt q) in
        take_while pred (snd p)
      else
        q
  | None -> q

let max n m = if (n < m) then m else n

let send_thread (val_ser[@metavar]) i socket_handler lock nodes outQueues acks =
  (* Dispatch messages in the out queue for node j.
     Returns the new queue. *)
  let send j q =
    if j = i then
      begin
        (* We don't send to ourselves, so the out-queue must be empty *)
        assert (queue_is_empty q);
        q
      end
    else
      let curr_ack = vect_nth !acks j in
      let q' = take_while (fun p ->
        let vc = snd p in
        let sn = vect_nth vc i in
        sn <= curr_ack) q
      in begin
        match (queue_peek_opt q') with
          Some p ->
            let vc = (snd p) in
            let sn = vect_nth vc i in
            assert (sn = curr_ack + 1);
            (* Message with seqnum curr_ack was acked, so now we can send seqnum curr_ack + 1.
              This could be the first time we're sending curr_ack + 1, or we could be
              retransmitting it because it hasn't been acked.
              Don't remove the message from the queue, since we might
              need to retransmit in the future.  *)
            let msg = InjR (p, i) in
            let dest = unSOME (list_nth nodes j) in
            ignore(sendTo socket_handler (msg_ser val_ser msg) dest)
        | None -> ()
      end;
      q'
  in
  loop_forever (fun () ->
    acquire lock;
    outQueues := list_mapi send !outQueues;
    release lock;
    Thread.delay 5.0)

let send_ack socket_handler sn rid dest_addr =
  let ack = InjL (sn, rid) in
  (* Since we're serializing an InjL, we can pass in a dummy 'val_ser'
     of 'unit_ser' *)
  let ack_raw = msg_ser unit_ser ack in
  ignore(sendTo socket_handler ack_raw dest_addr)

let recv_thread (val_deser[@metavar]) i socket_handler lock addrlst inQueue acks my_vc seen =
  let receive msg =
    match msg with
    | InjL p -> (* an ack: (sn, rid) *)
        let sn = fst p in
        let rid = snd p in
        let curr_ack = vect_nth !acks rid in
        acks := vect_update !acks rid (max curr_ack sn)
    | InjR p ->
      let payload = fst (fst p) in
      let vc = snd (fst p) in
      let rid = snd p in
      (* A broadcast message *)
      let their_sn = vect_nth vc rid in
      let their_addr = unSOME (list_nth addrlst rid) in
      let recorded_sn = vect_nth !my_vc rid in
      if (their_sn = recorded_sn + 1) then
        begin
          (* This is the next message we expect to receive from them.
             Only add it to the in-queue if we haven't seen it before. *)
           let is_present = unSOME (list_nth !seen rid) in
           if (not is_present) then
             (seen := list_update !seen rid true;
             inQueue := list_cons ((payload, vc), rid) !inQueue)
        end;
      if (their_sn <= recorded_sn + 1) then
        (* Ack the message even if it's old: our previous ack
           might have gotten lost. *)
        send_ack socket_handler their_sn i their_addr
      else ()
  in
  loop_forever (fun () ->
    let msg_raw = fst (unSOME (receiveFrom socket_handler)) in
    let msg = msg_deser val_deser msg_raw in
    acquire lock;
    receive msg;
    release lock)

let is_causally_next vc my_rid =
  let l = list_length vc in
  fun ev ->
    let ev_vc = snd (fst ev) in
    let origin = snd ev in
    if my_rid = origin then
      false
    else if origin < l then
      vect_applicable ev_vc vc origin
    else
      false

let deliver vc lock inQueue seen rid () =
  acquire lock;
  let remRes = list_find_remove (is_causally_next !vc rid) !inQueue in
  let res =
    match remRes with
    | Some p ->
      let msg = fst p in
      let origin = snd msg in
      let rest = snd p in
      inQueue := rest;
      seen := list_update !seen origin false;
      vc := vect_inc !vc origin;
      Some msg
    | None -> None
  in
  release lock;
  res

let broadcast vc outQueues lock rid payload =
  acquire lock;
  vc := vect_inc !vc rid;
  let msg = (payload, !vc) in
  outQueues := list_mapi (fun j q ->
    if (j <> rid) then queue_add msg q
    else q) !outQueues;
  release lock;
  (msg, rid)

(* Initializes RCB with a list of addresses [addrlst], where the current
   node sends and receives at address [i].
   Returns two closures for reading (deliver) and writing (broadcast) messages
   at address [i]. *)
let rcb_init (val_ser[@metavar]) (val_deser[@metavar]) addrlst i =
  let n = list_length addrlst in
  let vc = ref (vect_make n 0) in
  let acks = ref (vect_make n 0) in
  let seen = ref (list_make n false) in
  let inQueue = ref list_nil in
  let outQueues = ref (list_make n (queue_empty ())) in
  let lock = newlock () in
  let socket_handler = socket PF_INET SOCK_DGRAM IPPROTO_UDP in
  let addr = unSOME (list_nth addrlst i) in
  socketBind socket_handler addr;
  fork (send_thread val_ser i socket_handler lock addrlst outQueues) acks;
  fork (recv_thread val_deser i socket_handler lock addrlst inQueue acks vc) seen;
  (deliver vc lock inQueue seen i, broadcast vc outQueues lock i)
