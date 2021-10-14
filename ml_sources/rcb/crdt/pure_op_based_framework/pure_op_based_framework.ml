open List_code
open Set_code
open Vector_clock_code

let effectFW rel rel0 rel1 mes stateRef =
  let relMesStateBool = rel mes !stateRef in
  let newSet = list_filter (fun x -> ((not (rel0 x mes)) && relMesStateBool) 
  || ((not (rel1 x mes)) && (not(relMesStateBool)))) !stateRef in 
  if not (relMesStateBool) then stateRef := (set_add mes newSet) else stateRef := newSet  