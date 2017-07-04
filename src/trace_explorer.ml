type t = {
  trace : Trace.step array ;
  model : Model.t ;

  mutable grid_tests : (Grid.constr list) array ;
  mutable grid_actions : (Grid.constr list) array ;
  mutable has_grid : bool ;
}

let trace_size t = Array.length t.trace

let model t = t.model

let init model trace = {
  model ;
  trace ;
  grid_tests = [||] ;
  grid_actions = [||] ;
  has_grid = false ;
}

let array_of_queue q =
  let n = Queue.length q in
  Array.init n (fun _ -> Queue.pop q)

let load_from_file f =
  let q = Queue.create () in
  let model, () = 
    Trace.fold_trace_file (fun _ _ s ->
      Queue.push s q
    ) (fun _ -> ()) f in
  init model (array_of_queue q)

let fold ~init f t = 
  Tools.array_fold_lefti (fun i s acc -> f i acc s) init t.trace


module Grid = struct

  let tests i t = t.grid_tests.(i)

  let actions i t = t.grid_actions.(i)

  open Grid

  (* Functions to update the global state *)

  let init_global_state size = Hashtbl.create size

  let update_global_state constrs st =
    constrs |> List.iter
      (fun (Constr (x, v)) -> Hashtbl.replace st (Var x) (Constr (x, v)))

  let query_global_state var st : constr option =
    match Hashtbl.find_all st var with
    | []  -> None
    | [c] -> Some c
    | _   -> assert false


  let build_grid t =

    let n = trace_size t in
    let grid_tests = Array.make n [] in
    let grid_actions = Array.make n [] in
    let global_state = init_global_state (n / 4) in

    let write_row i pres mods =
      update_global_state mods global_state ;
      grid_tests.(i) <- pres ;
      grid_actions.(i) <- mods in

    let query_state v = query_global_state v global_state in

    let process_event i ev = 
      let pres = translate_tests ~query_state (List.concat ev.Instantiation.tests) in
      let mods_std = translate_actions ev.Instantiation.actions in
      let mods_side =
          List.map (fun site -> Constr (Binding_state site, Free)) ev.Instantiation.side_effects_dst in
      let mods = mods_std @ mods_side in
      write_row i pres mods in

    let process_step i = function
      | Trace.Subs _ | Trace.Dummy _ -> ()
      | Trace.Init actions ->
        let mods = translate_actions actions in
        write_row i []  mods
      | Trace.Rule (rule_id, ev, _) -> 
        process_event i ev
      | Trace.Obs (_, tests, _) ->
        write_row i (translate_tests ~query_state (List.concat tests)) []
      | Trace.Pert (_, ev, _) -> process_event i ev
    in

    Array.iteri process_step t.trace ;
    t.has_grid <- true ;
    t.grid_actions <- grid_actions ;
    t.grid_tests <- grid_tests

end