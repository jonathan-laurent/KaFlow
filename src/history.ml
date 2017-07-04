module type IMPL = sig

  type 'a t 

  val init : int -> 'a t

  val empty : 'a t

  val size : 'a t -> int

  val range : 'a t -> int

  val add : int -> 'a -> 'a t -> 'a t

  val last_before : int -> 'a t -> (int * 'a) option

  val first_after : int -> 'a t -> (int * 'a) option

end

module type S = sig

  include IMPL

  val add_list : (int * 'a) list -> 'a t -> 'a t

end

(* Sorted list implementation *)
module ListImpl : IMPL = 
struct
  type 'a t = (int * 'a) list

  let empty = []

  let init n = empty

  let size = List.length

  let range l = 
    try
      let (a, _) = List.hd l in
      let (b, _) = List.hd (List.rev l) in
      b - a
    with Invalid_argument _ | Not_found -> 0

  let rec add k v = function
    | [] -> [(k, v)]
    | (k', v')::kvs ->
      if k < k' then (k, v) :: (k', v') :: kvs
      else if k = k' then (k, v) :: kvs
      else (k', v') :: add k v kvs

  let rec last_before i = function
    | [] -> None
    | [k, v] -> if k < i then Some (k, v) else None
    | (k1, v1) :: (k2, v2) :: kvs ->
      if k2 >= i then
        if k1 < i then Some (k1, v1) else None
      else last_before i ((k2, v2) :: kvs)
  
  let rec first_after i = function
    | [] -> None
    | [k, v] -> if k > i then Some (k, v) else None
    | (k1, v1) :: (k2, v2) :: kvs ->
      if k1 >= i then
        if k2 > i then Some (k2, v2) else None
      else first_after i ((k2, v2) :: kvs)

end

(* Tree implementation *)
module TreeImpl : IMPL =
struct

  type 'a t =
    (* [Cat a b c left right]:
      Elements indexed in the [a,b) interval are in the left child
      Elements indexed in the [b, c) interval are in the right child *)
    | Cat of int * int * int * 'a t * 'a t
    | Leaf of int * 'a  (* [Node index object] *)
    | Empty

  let rec size = function
    | Cat (_, _, _, l, r) -> size l + size r
    | Leaf _ -> 1
    | Empty -> 0

  let range = function
    | Cat (a, _, c, _, _) -> c - a
    | Leaf _ -> 1
    | Empty -> 0

  let init' (a, b) =
    let sep = (a + b) / 2 in
    Cat (a, sep, b, Empty, Empty)

  let init size = init' (0, size)

  let empty = Empty

  type interval =
    | Null
    | Singleton of int
    | Interval of int * int

  let view_interval (a, b) : interval =
    if b - a <= 0 then Null
    else if b - a = 1 then Singleton a
    else Interval (a, b)

  let in_interval i (a, b) = i >= a && i < b


  let widen_init = 1024

  (* Note to myself: beware of unit tests ! *)

  let rec widen i = function
    | Empty -> widen i (init widen_init)
    | Cat (a, _, c, l, r) as node ->
      assert (a = 0) ; (* Must be called on the top node *)
      if in_interval i (a, c) then node
      else widen i (Cat (0, c, 2 * c, node, Empty))
    | Leaf _ -> assert false (* The top node cannot be a leaf node *)


  exception Out_of_bounds (* Should not be sent in the current implementation*)

  let add i x t =

    let rec add_aux (a, b) i x = function
      | Empty ->
        begin
          match view_interval (a, b) with
          | Null -> assert false
          | Singleton i' -> assert (i = i') ; Leaf (i, x)
          | Interval (a, b) -> add_aux (a, b) i x (init' (a, b))
        end
      | Leaf (i', _) -> assert (i = i') ; Leaf (i, x)
      | Cat (a', b', c', lc, rc) ->
        if in_interval i (a', b') then
          Cat (a', b', c', add_aux (a', b') i x lc, rc)
        else if in_interval i (b', c') then
          Cat (a', b', c', lc, add_aux (b', c') i x rc)
        else
          let open Printf in
          ( printf "%d %d %d %d" a' b' c' i ; raise Out_of_bounds) in

    let t = widen i t in
    add_aux (0, range t) i x t


  let rec last_before i = function
    | Empty -> None
    | Leaf (i', x) -> if (i' < i) then Some (i', x) else None
    | Cat (a, b, c, l, r) ->
      if i <= b then last_before i l
      else
        match last_before i r with
        | Some x -> Some x
        | None -> last_before i l


  let rec first_after i = function
    | Empty -> None
    | Leaf (i', x) -> if (i' > i) then Some (i', x) else None
    | Cat (a, b, c, l, r) ->
      if i >= b - 1 then first_after i r
      else
        match first_after i l with
        | Some x -> Some x
        | None -> first_after i r

end

(* Uses lists when there are few elements to store, trees otherwise *)
module Hybrid (L : IMPL) (H : IMPL) = 
struct

  type 'a impl =
    | Light of 'a L.t
    | Heavy of 'a H.t

  type 'a t = {
    lower_bound : int ;
    higher_bound : int ;
    size_leq : int ;
    impl : 'a impl ;
  }

end


module Lib (Impl : IMPL) = 
struct

  open Impl

  let add_list l t =
    List.fold_right (fun (i, x) acc -> add i x acc) l t

  let from_list l =
    if List.exists (fun (i, _) -> i < 0) l then
      raise (Invalid_argument "should only contain nonnegative indices")
    else add_list l empty

end



include TreeImpl
include Lib(TreeImpl)



(* Some tests *)

let zip_unit l = List.map (fun i -> (i, ())) l

let test_1 () =
  let t = from_list (zip_unit [0;1;3;4;6;8]) in
  assert (size t = 6) ;
  assert (first_after 4 t = Some (6, ())) ;
  assert (first_after 3 t = Some (4, ())) ;
  assert (last_before 3 t = Some (1, ())) ;
  assert (last_before 1 t = Some (0, ())) ;
  assert (last_before 0 t = None) ;
  assert (first_after 9 t = None) ;
  print_string "Success."


let test_2 () =
  let open Causal_core_util in
  let n = 100 in
  let max = 1000 in
  let l = random_int_list ~size:n max in
  let t = from_list (zip_unit l) in
  let l' =
    range_list (-1) (max + 1)
    |> List.map (fun i -> first_after i t)
    |> flatten_list_option
    |> List.map fst
  in
  assert (List.sort_uniq compare l = List.sort_uniq compare l') ;
  print_string "Success."