open Unix
open Ast
open Rcb_code
open Serialization_code
open Set_code
open List_code
open Gset_code

(** TODO: adapt the ml_sources/rcb/crdt/counter/counter_runner.ml file
    to run and test the gset_code handle inputs and outputs *)
let handle_io i elems size prepare = 
    let readString = read_line () in 
    match String.split_on_char ' ' readString with 
    | [ "add"; value ] -> (
        let () = prepare value in 
        Printf.printf "Node[%d] Added: %s\n" i value
    )
    | [ "elems" ] -> (
        Printf.printf "Node[%d] Set elements: " i;
        set_iter (Printf.printf "%s,") (elems ());
        Printf.printf "\n"
    )
    | [ "size" ] -> (
        Printf.printf "Node[%d] Set size is: %d\n" i (size ())
    )
    | _ -> ()

let init_exec () = 
    if Array.length Sys.argv < 4 then (
        prerr_endline "Usage: <index> <port1 port2 ... portN \n
    to read the elements of the set type 'elems', to add something to the set type 'add <value>'. To read the size of the set, write size";
        exit 2);
      let ip = string_of_inet_addr (gethostbyname "localhost").h_addr_list.(0) in
      let l =
        let sa i = SADDR (ip, (int_of_string Sys.argv.(i + 2))) in
        list_init (Array.length Sys.argv - 2) sa
      in
      let i = int_of_string Sys.argv.(1) in
      let pair = set_init l i in 
      let elems = fst pair in 
      let size = fst (snd pair) in
      let prepare = snd (snd pair) in
      loop_forever (fun () -> handle_io i elems size prepare)

let () = Unix.handle_unix_error init_exec ()