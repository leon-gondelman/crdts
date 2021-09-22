open Unix
open Ast
open Rcb_code
open List_code
open Dynamo_code
open Serialization_code
open Vector_clock_code

(* TODO: move somewhere else *)
let rec alist_to_list l = 
  match l with
    Some p -> (fst p) :: (alist_to_list (snd p))
  | None -> [] 

let vals_to_str vs = 
  let p_to_str p =
    "(" ^ (int_ser (fst p)) ^ "," ^ (vect_serialize (snd p)) ^ ")"
  in
  let body = String.concat ";" (alist_to_list (list_map p_to_str vs)) in
  "[" ^ body ^ "]"

let handle_io i rd upd =
  let s = read_line () in
  match String.split_on_char ' ' s with
  | [ "r"; k ] -> Printf.printf "CTR[%n] : %s\n" i (vals_to_str (rd k))
  | [ "w"; k; v_str ] ->
      let v = int_deser v_str in
      let () = upd k v in
      Printf.printf "CTR[%n] : %s\n" i (vals_to_str (rd k));
  | "close" :: _ -> exit 0
  | _ -> Printf.printf "invalid command \n"

let init_exec () =
  if Array.length Sys.argv < 4 then (
    prerr_endline "Usage: init <index> <port1 port2 ... portN>";
    exit 2);
  let ip = string_of_inet_addr (gethostbyname "localhost").h_addr_list.(0) in
  let l =
    let sa i = SADDR (ip, (int_of_string Sys.argv.(i + 2))) in
    list_init (Array.length Sys.argv - 2) sa
  in
  let i = int_of_string Sys.argv.(1) in
  let p = dynamo_init int_ser int_deser l i in
  let read = fst p in
  let read' = fun k ->
    match (read k) with
      Some vs -> vs
    | None -> list_nil
  in
  let update = snd p in
  loop_forever (fun () -> handle_io i read' update)

let () = Unix.handle_unix_error init_exec ()