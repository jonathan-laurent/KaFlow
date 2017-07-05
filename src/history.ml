type t = int array

let empty = [||]

let from_queue (q : int Queue.t) : t = 
  let n = Queue.length q in
  let t = Array.init n (fun _ -> Queue.pop q) in
  Array.fast_sort compare t ;
  t

let from_list l = 
  Array.of_list (List.sort_uniq compare l)

  
(*  Requires that [t] is sorted.
    Returns [(x, y) : int option * int option] where:
    * [x] is the largest index such that [t.(x) <= i]
    * [y] is the smallest index such that [t.(x) >= i] *)
let binary_search i t =

  let ret a b = 
    ( (if t.(b) <= i then Some b 
        else if t.(a) <= i 
        then Some a else None)
    , (if t.(a) >= i then Some a 
        else if t.(b) >= i 
        then Some b else None) ) in

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
  | None -> (* Every elt is > i *)
    if Array.length t > 0 then Some t.(0) else None
  | Some k -> 
    if t.(k) > i then assert false
    else if k+1 < Array.length t then Some t.(k+1)
    else None


let last_before i t =
  match snd (binary_search i t) with
  | None -> (* Every elt is < i *)
    let n = Array.length t in
    if n > 0 then Some t.(n-1) else None
  | Some k ->
    if t.(k) < i then assert false
    else if k-1 >= 0 then Some t.(k-1)
    else None


(* Some tests *)

let test_1 () =
  let t = from_list [0;1;3;4;6;8] in
  assert (first_after 4 t = Some 6) ;
  assert (first_after 3 t = Some 4) ;
  assert (last_before 3 t = Some 1) ;
  assert (last_before 1 t = Some 0) ;
  assert (last_before 0 t = None) ;
  assert (first_after 9 t = None) ;
  print_string "Success."


let test_2 () =
  let open Causal_core_util in
  let n = 100 in
  let max = 1000 in
  let l = random_int_list ~size:n max in
  let t = from_list l in
  let l' =
    range_list (-1) (max + 1)
    |> List.map (fun i -> first_after i t)
    |> flatten_list_option
  in
  assert (List.sort_uniq compare l = List.sort_uniq compare l') ;
  print_string "Success."