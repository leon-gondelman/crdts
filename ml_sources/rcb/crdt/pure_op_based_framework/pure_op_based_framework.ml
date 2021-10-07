open List_code
open Set_code
open Vector_clock_code

let effect rel rel0 rel1 mes stateRef =
  let relMesStateBool = rel mes !stateRef in
  let newSet = list_filter (fun x -> ((not (rel0 x mes)) && relMesStateBool) || ((not (rel1 x mes)) && (not(relMesStateBool)))) !stateRef in 
  if not (relMesStateBool) then !stateRef = (set_add mes newSet) else !stateRef = newSet

let rec vcEqual vc1 vc2 = match (vc1, vc2) with
  | (Some a, Some b) -> if ((fst a) = (fst b)) then (vcEqual (snd a) (snd b)) else false
  | (_, _) -> true

let stable stabilize vcLen ts stateRef =
  let vcBottom = (vect_make vcLen (-1)) in
  let newSet = stabilize ts stateRef in
  !stateRef = list_map (fun x -> if (vcEqual ts x) then vcBottom else x) newSet