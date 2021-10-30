open Ast
open Rcb_stabilise
open Serialization_code
open List_code
open Map_code
open Set_code
open Vector_clock_code
open Pure_op_based_framework
open Network_util_code
open Collaborative_editor_shared

(* Note messages are of the form: (((operation, (position, value)), vector clock), origin), 
  the set contains whole messages *)

let base = 1000000

let serializer = {s_ser = prod_ser string_ser (prod_ser (list_ser int_ser) string_ser); 
  s_deser = prod_deser string_deser (prod_deser (list_deser int_deser) string_deser)}

let prefix (list : int aset) (depth : int) = 
  let i = ref 1 in 
  let listcpy = ref list in
  let copyBuilder = ref list_nil in 
  while !i <= depth do 
    if !i < (list_length !listcpy) 
    then (copyBuilder := (list_append !copyBuilder (list_cons (unSOME (list_head !listcpy)) None));
    listcpy := (list_tail !listcpy); 
    i := (!i+1))
    else copyBuilder := list_append !copyBuilder (list_cons 0 list_nil); i:=!i+1
  done;
  !copyBuilder

let comparator m1 m2 = 
  let posList1 = getPosFromPayload m1 in 
  let posList2 = getPosFromPayload m2 in 
  let rec comparatorInner list1 list2 = 
    if list_head list1 = list_head list2 then comparatorInner (list_tail list1) (list_tail list2)
    else unSOME (list_head list1) < unSOME (list_head list2) in 
  comparatorInner posList1 posList2

let comparator' m1 m2 =  
  let posList1 : int aset = getPos m1 in 
  let posList2 : int aset = getPos m2 in 
  let rec comparatorInner list1 list2 = 
    if list_head list1 = list_head list2 then comparatorInner (list_tail list1) (list_tail list2)
    else unSOME (list_head list1) < unSOME (list_head list2) in 
  comparatorInner posList1 posList2

let rel (m1 : 'value msg) (s : 'value msg aset) =
  let rec concurrentAndLoss set = 
    match list_head set with
    | Some m2 ->
      (vect_conc_opt (getVC m2) (getVC m1) && getOr m2 < getOr m1)
      || concurrentAndLoss (list_tail set)
    | None -> false 
  in
  getOp m1 = "delete" || concurrentAndLoss s

let rel0 (m1 : 'value msg) (m2 : 'value msg) = 
  getOp m2 = "delete" && (getPos m1) = (getPos m2) && vect_leq_opt (getVC m1) (getVC m2)
  
let rel1 (m1 : 'value msg) (m2 : 'value msg) =
  vect_conc_opt (getVC m1) (getVC m2) && getOr m2 < getOr m1  

let subtract_positions p1 p2 =
  let rec inner p1 p2 =
    match list_head p1, list_head p2 with
    | Some a, Some b -> list_cons (a-b) (inner (list_tail p1) (list_tail p2))
    | None, None -> None
    | Some a, None -> list_cons (a) (inner (list_tail p1) None)
    | None, Some b -> list_cons (-b) (inner (None) (list_tail p2))     
  in
  list_rev (inner (list_rev p1) (list_rev p2))

let rec le_positions p1 p2 = 
  match list_head p1, list_head p2 with
  | Some a, Some b -> if a < b then le_positions (list_tail p1) (list_tail p2) else false
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> true

let rec le_one p = 
  match list_head p with
  | Some a -> if a < 1 then le_one (list_tail p) else false
  | None -> true    

(* This function assumes that we adding the position n on the last level 
or creating a new level and adding it. It also assymes that n is a single level position  *) 
let last_level_addition (p : int aset) (n : int aset) l =
  let value = list_nth p (l-1) in
  match value with
  | None -> list_append p n
  | Some a -> list_update p (l-1) (a)

let min_position_and_10000 p = 
  let reversed = list_rev p in
  let rec inner pos = 
    match list_head pos with
    | Some a -> if a > 0 then true else inner (list_tail pos)
    | None -> false
  in
  if inner (list_tail reversed) then list_cons 10000 None
  else (
    if unSOME (list_head reversed) > 10000 then list_cons 10000 None
    else list_cons (unSOME (list_head reversed)) None           
  )  


let compute_position state index = 
  if list_length state = 0 then (
    (list_iter (Printf.printf "%d\n") (list_cons (base/2) None));
    list_cons (base/2) None
  )
  else (  
    let indexInt = int_deser index in
    let sortedState = list_sort comparator' state in 
    let elemPrePos : (int aset) ref = ref None in
    let elemSucPos : (int aset) ref = ref None in 

    if indexInt = 0 then (
      elemPrePos := list_cons 0 None;
      let elementSuc = list_nth sortedState 0 in
      elemSucPos := getPos (unSOME elementSuc);
    ) else if indexInt = list_length sortedState then (
      elemSucPos := list_cons base None;
      let elementPre = list_nth sortedState (indexInt-1) in
      elemPrePos := getPos (unSOME elementPre); 
    ) else (
      let elementPre = list_nth sortedState (indexInt-1) in
      let elementSuc = list_nth sortedState indexInt in
      elemPrePos := getPos (unSOME elementPre);
      elemSucPos := getPos (unSOME elementSuc);
    );
    let interval = ref None in
    let depth = ref 0 in 
    while le_one !interval do 
      depth := !depth + 1;
      Printf.printf "depth: %d\n" !depth;
      interval := subtract_positions 
                (subtract_positions (prefix !elemSucPos !depth) (prefix !elemPrePos !depth)) 
                (list_cons 1 None);
    Printf.printf "prefix:\n";
    (list_iter (Printf.printf "%d\n") (!elemSucPos));
    flush_all ();                  
    done;
    let step = min_position_and_10000 !interval  in 
    Printf.printf "step:\n";
    (list_iter (Printf.printf "%d\n") (last_level_addition (prefix !elemPrePos !depth) step !depth));
    Printf.printf "return value:\n";
    (list_iter (Printf.printf "%d\n") (last_level_addition (prefix !elemPrePos !depth) step !depth));
    flush_all ();
    last_level_addition (prefix !elemPrePos !depth) step !depth
  )
                          
let get_position state (index : string) : int aset = 
  let indexInt = int_deser index in 
  let sortedState = list_sort comparator' state in
  getPos (unSOME (list_nth sortedState indexInt))

let known_queries =
  map_insert
    "read"
    (fun pset -> list_iter (fun m -> Printf.printf "(%s)" (snd (snd m))) 
    (list_sort comparator pset)) 
    (map_empty ())
  
let transformPayload state payload =
  let (operation, (index, value)) = payload in 
  if(not (is_valid_index state index)) then (None) else 
    if(fst payload = "write") then 
      (
      let newPosition = compute_position state index in 
      Some (operation, (newPosition, value))
      )
    else
      (
      let newPos : int aset = get_position state index in 
      Some (operation, (newPos, value))
      )
      
let editor_init addrs rid =  
  let (query, prepare) = crdt_init addrs rid serializer ((rel, rel0), rel1) known_queries stabilize in 
  (query, prepare (transformPayload))