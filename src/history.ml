type t = int array

let from_queue (q : int Queue.t) : t = 
  let n = Queue.length q in
  let t = Array.init n (fun _ -> Queue.pop q) in
  Array.fast_sort compare t ;
  t

  
(*  Requires that [t] is sorted.
    Returns [(x, y) : int option * int option] where:
    * [x] is the largest index such that [t.(x) <= i]
    * [y] is the smallest index such that [t.(x) >= i] *)
let binary_search i t =

  let ret a b = 
    ( (if t.(a) <= i then Some a else None)
    , (if t.(b) >= i then Some b else None) ) in

  let rec aux a b =
    if b < a then None, None
    else if b - a <= 1 then ret a b
    else
      let m = a + (b-a) / 2 in
      if t.(m) <= i then aux m b
      else aux a m

  in aux 0 (Array.length t - 1)


let first_after i t = 
  match fst (binary_search i t) with
  | None -> None
  | Some k -> 
    if t.(k) > i then Some t.(k)
    else if k+1 < Array.length t then Some t.(k+1)
    else None


let last_before i t =
  match snd (binary_search i t) with
  | None -> None
  | Some k ->
    if t.(k) < i then Some t.(k)
    else if k-1 > 0 then Some t.(k-1)
    else None