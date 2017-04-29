open Causal_core_shared
open Causal_core_util

type concrete_agent = Instantiation.concrete
type concrete_site  = Instantiation.concrete Instantiation.site
type internal_state = Instantiation.internal_state

type concrete_binding_state =
  | Free
  | Bound of concrete_site

type 'a var =
  | Internal_state  : concrete_site  -> internal_state var
  | Binding_state   : concrete_site  -> concrete_binding_state var
  | Agent_existence : concrete_agent -> bool var

type var' =
  | Var : 'a var -> var'

type constr =
  | Constr : 'a var * 'a -> constr

type constrs = constr list

type grid = (constrs * constrs) array (* tests, mods *)

type t = grid

let default : type a . Model.t -> a var -> a = fun env v ->
  match v with
  | Internal_state (ag, s) ->
    let opt_def =
      Signature.default_internal_state
        (Agent.sort ag) s (Model.signatures env) in
    begin
      match opt_def with
      | None -> assert false
      | Some def -> def
    end
  | Binding_state _ -> Free
  | Agent_existence _ -> false


let dumb_grid_row = ([], [])

let init_global_state size = Hashtbl.create size

let update_global_state constrs st =
  constrs |> List.iter
    (fun (Constr (x, v)) -> Hashtbl.replace st (Var x) (Constr (x, v)))

let query_global_state var st : constr option =
  match Hashtbl.find_all st var with
  | []  -> None
  | [c] -> Some c
  | _   -> assert false


let translate_action = function
   | Instantiation.Create (ag, site_inits) ->
    let tr_site (s, optst) =
      begin
        match optst with
        | Some st -> Some (Constr (Internal_state (ag, s), st))
        | None -> None
      end
    in
    Constr (Agent_existence ag, true) ::
    flatten_list_option (List.map tr_site site_inits)

  | Instantiation.Mod_internal (site, st) ->
    [Constr (Internal_state site, st)]
  | Instantiation.Bind (s, s') | Instantiation.Bind_to (s, s') ->
    [Constr (Binding_state s, Bound s') ; Constr (Binding_state s', Bound s)]
  | Instantiation.Free s ->
    [Constr (Binding_state s, Free)]
  | Instantiation.Remove ag ->
    [Constr (Agent_existence ag, false)]


let translate_test global_state = function

  | Instantiation.Is_Here ag -> [Constr (Agent_existence ag, true)]
  | Instantiation.Has_Internal (site, st) -> [Constr (Internal_state site, st)]
  | Instantiation.Is_Free site -> [Constr (Binding_state site, Free)]
  | Instantiation.Is_Bound_to (s, s') ->
    [Constr (Binding_state s, Bound s')]

  (* We freeze the exact bond that is used *)
  | Instantiation.Is_Bound site | Instantiation.Has_Binding_type (site, _) ->
    begin
      match query_global_state (Var (Binding_state site)) global_state with
      | None -> assert false
      | Some ((Constr (Binding_state _, Bound _)) as c) -> [c]
      | Some _  -> assert false
    end


let translate_actions actions =
  List.concat (List.map translate_action actions)

let translate_tests global_state tests =
  List.concat (List.map (translate_test global_state) tests)


let symmetrize_binding_constrs g =
  let sym (Constr (x, v) as c) =
    match x with
    | Binding_state s ->
      begin
        match v with
        | Free -> [c]
        | Bound s' -> [c ; Constr (Binding_state s', Bound s)]
      end
    | Agent_existence _ -> [c]
    | Internal_state _  -> [c] in

  let sym_cs cs = cs |> List.map sym |> List.concat |> List.sort_uniq compare in
  let aux (tests, mods) = (sym_cs tests, sym_cs mods) in
  Array.map aux g


let build_grid (_env : Model.t) (t : Trace.t) =

  let ta = Array.of_list t in
  let n = Array.length ta in
  let eois = Queue.create () in
  let grid = Array.make n dumb_grid_row in
  let global_state = init_global_state (n / 4) in

  let write_row i pres mods =
    update_global_state mods global_state ;
    grid.(i) <- (pres, mods) in

  let process_event i ev = 
    let pres = translate_tests global_state (List.concat ev.Instantiation.tests) in
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
    | Trace.Rule (_, ev, _) -> process_event i ev
    | Trace.Obs (_, tests, _) ->
      Queue.push i eois ;
      write_row i (translate_tests global_state (List.concat tests)) []
    | Trace.Pert (_, ev, _) -> process_event i ev
  in

  Array.iteri process_step ta ;
  (symmetrize_binding_constrs grid, list_of_queue eois)




open Format

let print_agent sigs f ag = 
  fprintf f "%a[%d]" 
    (Signature.print_agent sigs) (Agent.sort ag) (Agent.id ag)

let print_site sigs f (ag, site_id) =
  fprintf f "%a.%a"
    (print_agent sigs) ag 
    (Signature.print_site sigs (Agent.sort ag)) site_id

let print_cbs sigs f = function
  | Free -> fprintf f "free"
  | Bound site -> fprintf f "bound(%a)" (print_site sigs) site


let print_var env f (Var x) =
  let sigs = Model.signatures env in
  match x with
  | Internal_state site ->
    fprintf f "%a" 
      (print_site sigs) site 
  | Binding_state site ->
    fprintf f "%a" 
      (print_site sigs) site 
  | Agent_existence agent ->
    fprintf f "%a"
      (print_agent sigs) agent

let print_constr env eq_sym f (Constr (x, v)) =
  let sigs = Model.signatures env in
  match x with
  | Internal_state ((ag, s) as site) ->
    fprintf f "%a %s %a" 
      (print_site sigs) site 
      (eq_sym)
      (Signature.print_internal_state sigs (Agent.sort ag) s) v
  | Binding_state site ->
    fprintf f "%a %s %a" 
      (print_site sigs) site 
      (eq_sym)
      (print_cbs sigs) v
  | Agent_existence agent ->
    fprintf f "%a %s %b"
      (print_agent sigs) agent
      (eq_sym)
      (v)

let print_constrs env eq_sym = 
  Pp.list Pp.comma (print_constr env eq_sym)

let print_tests env = print_constrs env "="

let print_actions env = print_constrs env ":="

