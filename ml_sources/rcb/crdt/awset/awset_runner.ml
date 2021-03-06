open Unix
open Ast
open Rcb_minimal_code
open Serialization_code
open Set_code
open List_code
open Awset_code

let handle_io i read prepare = 
    let readString = read_line () in 
    match String.split_on_char ' ' readString with 
    | [ "add"; value ] -> (
      prepare ("add", value);
      Printf.printf "Node[%d] wrote: %s\n" i value
    )
    | [ "clear" ] -> (
      prepare ("clear", "");
      Printf.printf "Node[%d] cleared register\n" i
    )
    | [ "rmv"; value ] -> (
      prepare ("rmv", value);
      Printf.printf "Node[%d] removed: %s\n" i value
    )
    | [ "read" ] -> (
      Printf.printf "Node[%d] Read: " i;
      let q = match read "read" with Some x -> x | _ -> exit 0 in 
      set_iter (Printf.printf "%s,") q;
      Printf.printf "\n"
  )
    | _ -> ()

let init_exec () = 
    if Array.length Sys.argv < 4 then (
        Printf.printf "Usage: <index> <port1 port2 ... portN \n
    to read the register read, to write something to the register type 'write <value>'.\n";
        exit 2);
      let ip = string_of_inet_addr (gethostbyname "localhost").h_addr_list.(0) in
      let l =
        let sa i = SADDR (ip, (int_of_string Sys.argv.(i + 2))) in
        list_init (Array.length Sys.argv - 2) sa
      in
      let i = int_of_string Sys.argv.(1) in
      let pair = set_init l i in 
      let read = fst pair in 
      let prepare = snd pair in
      loop_forever (fun () -> handle_io i read prepare)

let () = Unix.handle_unix_error init_exec ()