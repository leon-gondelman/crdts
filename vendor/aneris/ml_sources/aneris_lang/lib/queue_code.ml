open! Ast
open List_code

(* O(1) amortized functional queue.
   A queue is a pair `(front, back)`, where we pop from `front`
   and push to `back` (which is stored in reverse order). *)

type 'a queue = 'a alist * 'a alist

let queue_empty () = (list_nil, list_nil)

let queue_is_empty q =
  match (fst q) with
    Some _p -> false
  | None ->
    match (snd q) with
      Some _p -> false
    | None -> true

let queue_add e q = (fst q, list_cons e (snd q))

let queue_norm q =
  match (fst q) with
    Some _p -> q
  | None -> (list_rev (snd q), list_nil)

let queue_peek_opt q =
  let q' = queue_norm q in
  match (fst q') with
    Some p -> Some (fst p)
  | None -> None

let queue_take_opt q =
  let q' = queue_norm q in
  match (fst q') with
    Some p -> Some (fst p, (snd p, snd q'))
  | None -> None
