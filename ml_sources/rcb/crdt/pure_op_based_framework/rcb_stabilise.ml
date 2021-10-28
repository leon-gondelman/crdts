include Rcb_minimal_code
open List_code
open Vector_clock_code
open Set_code

(* TODO: can we move this to VC code? *)
let rec vect_eq v1 v2 =
  match v1 with
    Some a1 ->
      (match v2 with
         Some a2 ->
           (fst a1 = fst a2) && vect_eq (snd a1) (snd a2)
       | None -> false)
  | None -> list_is_empty v2
let vect_bottom_test v =
  match v with
  | Some _ -> false
  | None -> true
let vect_conc_opt v1 v2 =
  match v1 v2 with 
  | None, None -> assert false (* User of the CRDT should prevent this case *)
  | Some a, Some b -> vect_conc a b   
  | _, _ -> false 
let vect_leq_opt v1 v2 =   
  match v1 v2 with 
  | None, None -> assert false (* User of the CRDT should prevent this case *)
  | Some a, Some b -> vect_leq a b   
  | Some _, None -> false 
  | None, Some _ -> true 
let vect_eq_opt v1 v2 =
  match v1 v2 with 
  | None, None -> assert false (* User of the CRDT should prevent this case *)
  | Some a, Some b -> vect_eq a b   
  | _, _ -> false 

let vect_nth_opt v n =
  match v with 
  | None -> None
  | Some a -> vect_nth a n

let low_function local_map src = 
  let vc = match list_nth !local_map 0 with | Some a -> !a | None -> exit 1 in
  let res = ref (vect_nth vc src) in
  list_iter (fun x -> 
    let current_value = vect_nth (!x) src in
    if current_value < !res then res := current_value) !local_map;
  !res 

let stable stabilize stateRef stableMessage =
  let newSet = stabilize stableMessage !stateRef in
  stateRef := list_map (fun message -> if (vect_eq_opt (snd (fst stableMessage)) (snd (fst message))) 
    then ((fst (fst (fst message)), snd (fst (fst message))), None), snd message else message) newSet

let stabilizing_deliver deliver local_map set stabilize_function () = match (deliver ()) with
    | Some message -> 
      (
      let vcRefOptional = (list_nth !local_map (snd message)) in   
      let vcRef = match vcRefOptional with | Some a -> a | None -> exit 1 in
      vcRef := (snd (fst message));
      let stablePred = (fun message -> vect_nth_opt (snd (fst message)) (snd message) <= low_function local_map (snd message)) in 
      let stableSet = list_filter (fun x -> stablePred x) !set in
      list_iter (stable stabilize_function set) stableSet;
      if (vect_eq_opt (Some (snd (fst message))) None) then Some ((fst (fst message), None), snd message) else
      Some ((fst (fst message), Some (snd (fst message))), snd message)
      ) 
    | None -> None

let rcb_init (val_ser[@metavar]) (val_deser[@metavar]) addrlst i set stabilize_function =
  let pair = Rcb_minimal_code.rcb_init val_ser val_deser addrlst i in
  let deliver = fst pair in
  let broadcast = snd pair in
  let n = list_length addrlst in
  let local_map = ref (list_make n (ref (vect_make n 0))) in 
  (stabilizing_deliver deliver local_map set stabilize_function, broadcast)