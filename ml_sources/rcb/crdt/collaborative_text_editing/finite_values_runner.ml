open Unix
open Ast
open Rcb_code
open Serialization_code
open Set_code
open List_code
open Finite_values_code

let handle_io i query prepare = 
    let readString = read_line () in 
    match String.split_on_char ' ' readString with 
    | [ "write"; position; value] -> (
        if (0.0 <= float_of_string position && float_of_string position <= 1.0 ) then (
          Printf.printf "Node[%d] wrote: %s at position %s\n" i value position;
          prepare ("write", (position, value));
        ) else (
          Printf.printf "Position must be between 0 and 1\n";
        ); 
       flush_all ();
    )
    | [ "delete"; position] -> (
        Printf.printf "Node[%d] deleted charcater at position %s\n" i position;
        prepare ("delete", (position, ""));
        flush_all ();
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
      let pair = editor_init l i in 
      let query = fst pair in 
      let prepare = snd pair in

      Printf.printf "----------- Welcome To The Finite Collaborative Text Editor  -----------\n";
      Printf.printf " - 'read' to read the text\n"; 
      Printf.printf " - 'write x y' to write 'y' at position 'x'\n";
      Printf.printf " - positions must be in the interval [0;1]\n"; 
      Printf.printf "-------------------------------------------------------------------------\n\n";

      loop_forever (fun () -> handle_io i query prepare)

let () = Unix.handle_unix_error init_exec ()
