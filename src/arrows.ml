open Causal_core_shared
open Causal_core_util

open Grid

let constr_var (Constr (x, _v)) = Var x

let compute_precedence te (core : step_id list) =
  let sub = core in
  let varmod = Hashtbl.create (List.length sub) in
  sub |> List.iter (fun i ->
      let mods = Trace_explorer.Grid.actions i te in
      mods |> List.iter (fun (Constr (x, _v)) ->
          Hashtbl.add varmod (Var x) i )) ;
  sub |> List.map (fun i ->
      let mods = Trace_explorer.Grid.actions i te in
      let tests = Trace_explorer.Grid.tests i te in
      let touched_vars = List.map constr_var (tests @ mods) in
      touched_vars
      |> List.map (fun x ->
          Hashtbl.find_all varmod x |> List.map (fun j ->
              if i <> j then Some (min i j, max i j) else None ))
      |> List.concat
      |> flatten_list_option )
  |> List.concat
  |> List.sort_uniq compare

(* Also checks that the core is valid *)

let compute_dependencies_generic
    ~strong_only
    (te : Trace_explorer.t)
    (core : step_id list) =

  let deps = Queue.create () in
  let state = Hashtbl.create (List.length core) in
  let env = Trace_explorer.model te in

  let process_event i =
    let mods = Trace_explorer.Grid.actions i te in
    let tests = Trace_explorer.Grid.tests i te in

    tests |> List.iter (fun (Constr (x, v) as c) ->
        try
          let (st, src) = Hashtbl.find state (Var x) in
          if (st <> c) then
            begin
              let printer = Grid.print_constr env "=" in
              Format.printf "Mismatch:@;%a@;%a@;" printer st printer c;
              assert false
            end ;
          if not (v = Grid.default env x && strong_only) then
            Queue.push (i, c, src) deps
        with Not_found ->  assert false ) ;

    mods |> List.iter (fun (Constr (x, _v) as c) ->
        Hashtbl.replace state (Var x) (c, i) ) in

  List.iter process_event core;
  list_of_queue deps

let compute_strong_dependencies =
  compute_dependencies_generic ~strong_only:true

let compute_dependencies te core =
  compute_dependencies_generic ~strong_only:false te core
  |> List.map (fun (dest, _constr, src) -> (src, dest))

(* Transitive reduction *)

module G = Graph.Make(Int)

let transitive_reduction prec =
  prec
  |> G.from_edges_list
  |> G.transitive_reduction
  |> G.to_edges_list

let set_difference edges to_remove =
  let to_remove_set = Hashtbl.create (List.length to_remove) in
  to_remove |> List.iter (fun e -> Hashtbl.add to_remove_set e ());
  edges |> List.filter (fun e -> not (Hashtbl.mem to_remove_set e))