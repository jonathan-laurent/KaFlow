open Causal_core_shared
open Causal_core_util

open Grid

module Int_set = Set.Make (Int)

module Int_map = Map.Make (Int)

(******************************************************************************)
(* Variables information cache                                                *)
(******************************************************************************)

type var_info = {
  modified_in_t : History.t ;
  set_to_def_in_t : History.t ;
  mutable last_tested_to_def_in_sigma : step_id ;
  mutable potential_closing_deps : step_id list
}

type var_info_table = (var', var_info) Hashtbl.t


let def_var_info () = {
  modified_in_t = History.empty ;
  set_to_def_in_t = History.empty ;
  last_tested_to_def_in_sigma = -1 ; (* Never tested to def *)
  potential_closing_deps = []
}

let init_var_info modified_in_t set_to_def_in_t = {
  modified_in_t = History.from_queue modified_in_t ;
  set_to_def_in_t = History.from_queue set_to_def_in_t ;
  last_tested_to_def_in_sigma = -1 ; (* Never tested to def *)
  potential_closing_deps = []
}

let clear_var_infos t = 
  t |> Hashtbl.iter (fun _ infos ->
    infos.last_tested_to_def_in_sigma <- (-1) ;
    infos.potential_closing_deps <- [] ;
  )

let get_var_info x tbl =
  try Hashtbl.find tbl (Var x)
  with Not_found ->
    begin
    let infos = def_var_info () in
    Hashtbl.add tbl (Var x) infos ;
    infos
    end

let add_potential_closing_dep x dep tbl =
  let infos = get_var_info x tbl in
  infos.potential_closing_deps <- dep :: infos.potential_closing_deps

let set_potential_closing_deps x deps tbl =
  let infos = get_var_info x tbl in
  infos.potential_closing_deps <- deps

(* Returns true if there was an effective update *)
let update_last_tested_to_def i x tbl =
  let infos = get_var_info x tbl in
  if i > infos.last_tested_to_def_in_sigma then
  begin
    infos.last_tested_to_def_in_sigma <- i ;
    true
  end
  else false


(* Cache some informations on how variables are modified *)

let init_var_infos ?last_step_id te =

  let last_step = 
    match last_step_id with
    | None -> Trace_explorer.last_step_id te
    | Some s -> s in

  let env = Trace_explorer.model te in
  let cache = Hashtbl.create (last_step / 4) in
  let get x = 
    try Hashtbl.find cache (Var x)
    with Not_found ->
      let v = (Queue.create (), Queue.create ()) in
      begin Hashtbl.add cache (Var x) v ; v end in

  for i = 0 to last_step do
    let mods = Trace_explorer.Grid.actions i te in    
    mods |> List.iter (fun (Constr (x, v)) ->
        let mod_in_t, set_def_in_t = get x in
        Queue.push i mod_in_t ;
        if v = Grid.default env x then
          Queue.push i set_def_in_t )
  done ;

  let var_infos = Hashtbl.create (last_step / 4) in
  cache |> Hashtbl.iter (fun x (mod_in_t, set_def_in_t) ->
    Hashtbl.add var_infos x (init_var_info mod_in_t set_def_in_t)
  ) ;
  var_infos





(******************************************************************************)
(* Main algorithm                                                             *)
(******************************************************************************)

(*

The algorithm to compute causal cores is greedy and relies on the
regularity hypothesis. It works by progressively
expanding a subset of events `sigma` as follows:

Initially, sigma only contains the event of interest.
Then, it grows by the two following rules:
[Ra] If an event is in sigma, its strong dependencies should be added to sigma
[Rd] If an event e altering a variable x is in sigma and there is an ulterior
event in sigma testing x to its default value, then add to sigma the first
event after e restoring x to its default value.

We stop when no rule can apply. Besides, we can prove the rules are confluent.

Lemma: if no rule can be applied, then sigma yields a valid subtrace
Proof: it is enough to prove that every dependency is respected.
This is trivial for strong dependencies. Then, let's suppose
event e in sigma requires x=0. Then, let's look at the last event e' in sigma
before e modifying x (it necesessarily exists, even if it is `init`).
If it sets x to 0, we're done. Otherwise, the next event in t altering
x should be in sigma (because it has to read x by regularity and so 
e' strongly depends on it).  Because t is valid, this event should come before
e. Contradiction.

Notes:
  + Init events are necessarily selected because there are logical sites for
    agent existence.

Lemma: if no rule can be applied, sigma strongly matches t (needs regularity)

*)

type sigma_event_info = {
  number_added : int
}

type causal_core = (step_id * sigma_event_info) list

type t = causal_core

let compute_causal_core
    (te : Trace_explorer.t)
    (var_infos : var_info_table) (* Not modified *)
    (eois : step_id list) =

  let env = Trace_explorer.model te in

  clear_var_infos var_infos ;

  let strong_deps : step_id Queue.t = Queue.create () in
  let closing_deps : step_id Queue.t = Queue.create () in
  let sigma : sigma_event_info Int_map.t ref = ref (Int_map.empty) in
  let counter : int ref = ref 0 in

  let add_to_sigma i =
    if not (Int_map.mem i !sigma) then
    begin

      (* Add into the sigma set *)
      sigma := Int_map.add i {number_added = !counter} !sigma;
      counter := !counter + 1 ;

      let mods = Trace_explorer.Grid.actions i te in
      let tests = Trace_explorer.Grid.tests i te in

      tests |> List.iter (fun (Constr (x, v)) ->

          let infos = get_var_info x var_infos in

          (* Look for strong dependencies *)
          if v <> Grid.default env x then
            match History.last_before i infos.modified_in_t with
            | None -> ()
            | Some dep -> Queue.push dep strong_deps

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
            match History.first_after i infos.set_to_def_in_t with
            | None -> ()
            | Some clos ->
              if infos.last_tested_to_def_in_sigma > clos then
                (* Add as a closing dependency *)
                Queue.push clos closing_deps
              else
                (* Add in the waiting list *)
                add_potential_closing_dep x clos var_infos
        )
    end in

  List.iter add_to_sigma eois ;

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
      Int_map.fold (fun i info acc -> (i, info) :: acc) !sigma [] in
  List.sort (fun x y -> compare (fst x) (fst y)) unordered_core




let iter_causal_cores
    (te : Trace_explorer.t) (eois : step_id list)
    (handle : step_id -> causal_core -> unit) =

  let last_step_id = list_maximum eois in

  let var_infos = init_var_infos ~last_step_id te in

  eois |> List.iter (fun eoi ->
      handle eoi (compute_causal_core te var_infos [eoi]) ;
  )

 let core_events = List.map fst
