open Causal_core_util

open Grid

let constr_var (Constr (x, _v)) = Var x

let compute_precedence (t : Trace.t) (g : grid) (core : Causal_core.causal_core) =
  let sub = Causal_core.core_events core in
  let varmod = Hashtbl.create (List.length sub) in
  sub |> List.iter (fun i ->
      let (_tests, mods) = g.(i) in
      mods |> List.iter (fun (Constr (x, _v)) ->
          Hashtbl.add varmod (Var x) i )) ;
  sub |> List.map (fun i ->
      let (tests, mods) = g.(i) in
      let touched_vars = List.map constr_var (tests @ mods) in
      touched_vars 
      |> List.map (fun x -> 
          Hashtbl.find_all varmod x |> List.map (fun j ->
              if i <> j then Some (min i j, max i j) else None ))
      |> List.concat
      |> flatten_list_option )
  |> List.concat 
  |> List.sort_uniq compare

module G = Graph.Make(Int)

let transitive_reduction prec = 
  prec 
  |> G.from_edges_list 
  |> G.transitive_reduction
  |> G.to_edges_list


(* Also checks that the core is valid *)

let compute_strong_deps
    (env : Model.t) 
    (g : grid) 
    (core : Causal_core.causal_core) =

  let deps = Queue.create () in
  let state = Hashtbl.create (List.length core) in

  let process_event i = 
    let (tests, mods) = g.(i) in

    tests |> List.iter (fun (Constr (x, v) as c) ->
        try
          let (st, src) = Hashtbl.find state (Var x) in
          if (st <> c) then
            begin
              let printer = Grid.print_constr env "=" in
              Format.printf "Mismatch:@;%a@;%a@;" printer st printer c;
              assert false
            end ;
          if v <> Grid.default env x then
            Queue.push (i, c, src) deps
        with Not_found ->  assert false ) ;

    mods |> List.iter (fun (Constr (x, _v) as c) ->
        Hashtbl.replace state (Var x) (c, i) ) in

  List.iter process_event (Causal_core.core_events core);
  list_of_queue deps
