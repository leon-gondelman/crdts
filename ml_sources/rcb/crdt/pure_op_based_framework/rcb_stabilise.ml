include Rcb_code
open List_code
open Vector_clock_code

let low_function local_map src = 
  let res = ref (vect_nth (list_nth !local_map 0) src) in
  list_iter (fun x -> 
    let current_value = vect_nth (!x) src in
    if current_value < !res then res := current_value) !local_map;
  !res 

let rec vcEqual vc1 vc2 = match (vc1, vc2) with
  | (Some a, Some b) -> if ((fst a) = (fst b)) then (vcEqual (snd a) (snd b)) else false
  | (_, _) -> true

let stable stabilize vcLen stateRef ts =
  let vcBottom = (vect_make vcLen (-1)) in
  let newSet = stabilize ts stateRef in
  stateRef := list_map (fun x -> if (vcEqual ts x) then vcBottom else x) newSet

let stabilizing_deliver deliver local_map set stabilize_function vcLen = match (deliver ()) with
    | Some message -> 
      (
      let vcRef = (list_nth !local_map (snd message)) in   
      vcRef := (snd (fst message));
      let stablePred = (fun message -> vect_nth (snd (fst message)) (snd message) <= low_function local_map (snd message)) in 
      let stableSet = list_filter (fun x -> stablePred x) !set in
      list_iter (stable stabilize_function vcLen set) stableSet;
      (Some message)
      ) 
    | None -> None

let rcb_init (val_ser[@metavar]) (val_deser[@metavar]) addrlst i set stabilize_function =
  let pair = Rcb_code.rcb_init val_ser val_deser addrlst i in
  let deliver = fst pair in
  let broadcast = snd pair in
  let n = list_length addrlst in
  let local_map = ref (list_make n (ref (vect_make n 0))) in 
  (stabilizing_deliver deliver local_map set n stabilize_function, broadcast)
  
 