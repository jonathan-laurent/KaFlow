open Causal_core_shared
open Causal_core_util

open Grid

module Int_set = Set.Make (Int)

module Int_std_map = Map.Make (Int)

(******************************************************************************)
(* Variables information cache                                                *)
(******************************************************************************)

type var_info = {
  modified_in_t : unit Int_map.t ;
  set_to_def_in_t : unit Int_map.t ;
  last_tested_to_def_in_sigma : step_id ;
  potential_closing_deps : step_id list
}

let def_var_info = {
  modified_in_t = Int_map.empty ;
  set_to_def_in_t = Int_map.empty ;
  last_tested_to_def_in_sigma = -1 ; (* Never tested to def *)
  potential_closing_deps = []
}

let get_var_info x tbl =
  try Hashtbl.find tbl (Var x)
    with Not_found -> def_var_info

let add_mod_in_t i x tbl =
  let old = get_var_info x tbl in
  Hashtbl.replace tbl (Var x)
    { old with modified_in_t = Int_map.add i () (old.modified_in_t)}

let add_set_def_in_t i x tbl =
  let old = get_var_info x tbl in
  Hashtbl.replace tbl (Var x)
    { old with set_to_def_in_t = Int_map.add i () (old.set_to_def_in_t)}

let add_potential_closing_dep x dep tbl =
  let old = get_var_info x tbl in
  let newer =
    { old with potential_closing_deps = dep :: old.potential_closing_deps} in
  Hashtbl.replace tbl (Var x) newer

let set_potential_closing_deps x deps tbl =
  let old = get_var_info x tbl in
  let newer =
    { old with potential_closing_deps = deps } in
  Hashtbl.replace tbl (Var x) newer

(* Returns true if there was an effective update *)
let update_last_tested_to_def i x tbl =
  let old = get_var_info x tbl in
  if i > old.last_tested_to_def_in_sigma then
    let newer =
      { old with last_tested_to_def_in_sigma = i} in
    Hashtbl.replace tbl (Var x) newer ;
    true
  else false

type var_info_table = (var', var_info) Hashtbl.t

(* Cache some informations on how variables are modified *)

let init_var_infos
    (env : Model.t)
    (g : grid)
    (last_step : step_id)
    (var_infos : var_info_table) =

  for i = 0 to last_step do
    let (_tests, mods) = g.(i) in
    mods |> List.iter (fun (Constr (x, v)) ->
        add_mod_in_t i x var_infos ;
        if v = Grid.default env x then
          add_set_def_in_t i x var_infos )
  done


(******************************************************************************)
(* Main algorithm                                                             *)
(******************************************************************************)

(*

The algorithm to compute causal cores is greedy. It works by progressively
expanding a subset of events sigma as follows:

Initially, sigma only contains the event of interest.
[Ra] If an event is in sigma, its strong dependencies should be added to sigma
[Rd] If an event e altering a variable x is in sigma and there is an ulterior
event in sigma testing x to its default value, then add to sigma the first
event after e restoring x to its default value.

We stop when no rule can apply. Besides, we can prove the rules are confluent.

Lemma: if no rule can be applied, then sigma yields a valid subtrace
Proof: it is enough to prove that every dependency is respected.
This is trivial for strong dependencies. Then, let's suppose
event e in sigma requires x=0. Then, let's look at the last event before
e in sigma modifying x (it necesessarily exists, even if it is `init`).
If it sets x to 0, we're done. Otherwise, the next event in t altering
x should be in sigma. Because t is valid, this event should come before
e. Contradiction.

Lemma: if no rule can be applied, sigma strongly matches t (needs regularity)

As opposed as what I once thought, no regularity hypothesis is needed.

*)

type sigma_event_info = {
  number_added : int
}

type causal_core = (step_id * sigma_event_info) list

type t = causal_core

let compute_causal_core
    (env : Model.t)
    (g : grid)
    (var_infos : var_info_table) (* Not modified *)
    (eoi : step_id) =

  let var_infos = Hashtbl.copy var_infos in

  let strong_deps : step_id Queue.t = Queue.create () in
  let closing_deps : step_id Queue.t = Queue.create () in
  let sigma : sigma_event_info Int_std_map.t ref = ref (Int_std_map.empty) in
  let counter : int ref = ref 0 in

  let add_to_sigma i =
    if not (Int_std_map.mem i !sigma) then
    begin

      (* Add into the sigma set *)
      sigma := Int_std_map.add i {number_added = !counter} !sigma;
      counter := !counter + 1 ;

      let (tests, mods) = g.(i) in
      tests |> List.iter (fun (Constr (x, v)) ->

          let infos = get_var_info x var_infos in

          (* Look for strong dependencies *)
          if v <> Grid.default env x then
            match Int_map.last_before i infos.modified_in_t with
            | None -> ()
            | Some (dep, ()) -> Queue.push dep strong_deps

          (* Update cdep waiting lists *)
          else
            if update_last_tested_to_def i x var_infos then
            (* the event added to sigma (i) is the last in sigma to
            tests x against a default value *)
              let to_add, still_waiting =
                List.partition (fun d -> d < i)
                  infos.potential_closing_deps in
              set_potential_closing_deps x still_waiting var_infos ;
              to_add |> List.iter (fun d -> Queue.push d closing_deps) ) ;

      (* Look for closing events *)
      mods |> List.iter (fun (Constr (x, v)) ->
          if v <> Grid.default env x then
            let infos = get_var_info x var_infos in
            match Int_map.first_after i infos.set_to_def_in_t with
            | None -> ()
            | Some (clos, ()) ->
              if infos.last_tested_to_def_in_sigma > clos then
                (* Add as a closing dependency *)
                Queue.push clos closing_deps
              else
                (* Add in the waiting list *)
                add_potential_closing_dep x clos var_infos
        )
    end in

  add_to_sigma eoi ;

  let continue = ref true in

  while !continue do
    try
      add_to_sigma (Queue.pop strong_deps) ;
    with Queue.Empty ->
      try
        add_to_sigma (Queue.pop closing_deps) ;
      with Queue.Empty -> continue := false
  done ;

  let unordered_core = 
      Int_std_map.fold (fun i info acc -> (i, info) :: acc) !sigma [] in
  List.sort (fun x y -> compare (fst x) (fst y)) unordered_core




let iter_causal_cores
    (env : Model.t) (g : grid) (eois : step_id list)
    (handle : step_id -> causal_core -> unit) =

  let last_eoi = list_maximum eois in

  let var_infos : (var', var_info) Hashtbl.t = Hashtbl.create (last_eoi / 4) in

  init_var_infos env g last_eoi var_infos ;

  eois |> List.iter (fun eoi ->
      handle eoi (compute_causal_core env g var_infos eoi) )


 let core_events = List.map fst