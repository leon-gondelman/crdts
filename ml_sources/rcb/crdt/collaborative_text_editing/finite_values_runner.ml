open Unix
open Ast
open Rcb_code
open Serialization_code
open Set_code
open List_code
open Finite_values_code

let handle_io i query prepare stateRef stateLock = 
    let readString = read_line () in 
    match String.split_on_char ' ' readString with 
    | [ "write"; index; value] -> (
       acquire stateLock; 
       if (is_valid_index !stateRef index) then (
        release stateLock; 
        prepare ("write", (compute_position !stateRef index, value));
        Printf.printf "Node[%d] wrote: %s at index %s\n" i value index;
        flush_all ();
       ) else (
        release stateLock; 
        Printf.printf "Invalid index\n";
        flush_all ();
       )
    )
    | [ "delete"; index] -> (
      acquire stateLock; 
      if (is_valid_index !stateRef index) then (
        release stateLock;
        prepare ("delete", (get_position !stateRef index, "")); 
        Printf.printf "Node[%d] deleted charcater at index %s\n" i index;
        flush_all ();
       ) else (
        release stateLock;  
        Printf.printf "Invalid index\n";
        flush_all ();
       )
    )
    | [ "read" ] -> (
        Printf.printf "Node[%d] Read: \n" i;
        let _ = match query "read" with Some x -> x | _ -> () in
        Printf.printf "\n";
        flush_all ();
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
      let utilities = editor_init l i in 
      let query = fst (snd utilities) in 
      let prepare = snd (snd utilities) in
      let stateRef = fst (fst utilities) in
      let stateLock = snd (fst utilities ) in

      Printf.printf "----------- Welcome To The Finite Collaborative Text Editor  -----------\n";
      Printf.printf " - 'read' to read the text\n"; 
      Printf.printf " - 'write x y' to write 'y' at index 'x'\n";
      Printf.printf " - 'delete x' to delete the character at index x\n"; 
      Printf.printf "-------------------------------------------------------------------------\n\n";

      loop_forever (fun () -> handle_io i query prepare stateRef stateLock)

let () = Unix.handle_unix_error init_exec ()
