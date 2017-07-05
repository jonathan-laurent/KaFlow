open Format
open Causal_core_util

let def_font = "CMU Serif"

let max_intro_len = 20

type nodes_coloring =
  | Build_order
  | Time

type print_options =
  { ranksep            : float  ;
    show_strong_deps   : bool   ;
    strong_deps_labels : bool   ;
    dump_grid          : bool   ;
    show_event_ids     : bool   ;
    font               : string ;
    nodes_coloring     : nodes_coloring  ;
  }

let def_options_detailed = 
  { ranksep            = 1.0   ;
    show_strong_deps   = true  ;
    strong_deps_labels = true  ;
    dump_grid          = true  ;
    show_event_ids     = true  ;
    font               = def_font ;
    nodes_coloring     = Time
  }

let def_options_simple =
  { ranksep            = 0.3    ;
    show_strong_deps   = false  ;
    strong_deps_labels = true   ;
    dump_grid          = false  ;
    show_event_ids     = false   ;
    font               = def_font ;
    nodes_coloring     = Time
  }

let print_hsv_color fmt (h, s, v) =
  fprintf fmt "\"%.3f %.3f %.3f\"" h s v

type hsv_color = float * float * float

type dot_annot = { 
  label : string ;
  shape : string ;
  style : string ;
  fillcolor : hsv_color }

let white = (0.0, 0.0, 1.0)

let def_annot = {
  label = "" ;
  shape = "invhouse" ;
  style = "filled" ;
  fillcolor = white }

let shorten_str max str = 
  let n = String.length str in
  if n > max then
    String.sub str 0 (min n (max - 3)) ^ "..."
  else
    str

let print_dot_annot fmt annot =
  fprintf fmt "[label=\"%s\", shape=%s, style=%s, fillcolor=%a]"
    annot.label annot.shape annot.style print_hsv_color annot.fillcolor

let intro_name env s = 
  asprintf "Intro @[<h>%a@]"
    (Pp.list Pp.comma (Model.print_agent ~env)) s
  |> shorten_str max_intro_len


let event_kind_dot_annot env = function
  | Trace.RULE r_id -> 
    { def_annot with 
      label = asprintf "%a" (Model.print_ast_rule ~env) r_id }
  (*| Trace.OBS obs_name ->
    { def_annot with label = obs_name ; shape = "rectangle" } *)
  | Trace.INIT s ->
    { def_annot with label = intro_name env s }
  | Trace.PERT s ->
    { def_annot with label = s ; shape = "invhouse" }


let introduced_agent_sorts actions_list = 
  actions_list 
  |> List.map (fun a ->
      match a with
      | Instantiation.Create (ag, _) -> Some (Agent.sort ag)
      | _ -> None )
  |> flatten_list_option


let print_event options te color_handle f (i, info) =

  let env = Trace_explorer.model te in 
  let pr x = Format.fprintf f x in

  let actions = Trace_explorer.Grid.actions i te in
  let tests = Trace_explorer.Grid.tests i te in
  
  if options.dump_grid then
    begin
    (*pr "/* %a */@;" (Trace.print_step ~compact:false ~env:env) ta.(i);*)
    pr "/* EVENT : %d@;" i ;
    pr "   TESTS : @[<hov>%a@]@;"
      (Grid.print_tests env) tests ;
    pr "   MODS  : @[<hov>%a@]@;*/@;" 
      (Grid.print_actions env) actions ;
    end ;
  pr "%d " i ;

  let annot = 
    begin
      match Trace_explorer.step i te with
      | Trace.Rule (rule_id, ev, _) ->
        { def_annot with 
        label = asprintf "%s" (rule_ast_name env rule_id) }
      | Trace.Obs (obs_name, _, _) ->
        { def_annot with label = obs_name ; shape = "rectangle" }
      | Trace.Init actions ->
        let agents = introduced_agent_sorts actions in
        { def_annot with label = intro_name env agents ; shape = "rectangle" }
      | Trace.Pert (s, _, _) ->
        { def_annot with label = s ; shape = "invhouse" }
      | Trace.Subs _ | Trace.Dummy _ -> def_annot
    end in

  let annot = 
    if options.show_event_ids then
      let decorated_name = sprintf "[%d] %s" i annot.label in
      { annot with label = decorated_name }
    else annot in 

  let annot = { annot with fillcolor = color_handle i info } in

  print_dot_annot f annot ;
  pr "@;@;"
  


let print_prec_arrow options fmt (src, dest) =
  fprintf fmt "%d -> %d [dir=none%s] @;" src dest
    (if options.show_strong_deps then ", color=grey" else "")


let important_constr c = 
  let open Grid in
  match c with
  | Constr (Agent_existence _, _) -> false
  | _ -> true

let print_strong_dep_arrow options env fmt (dest, constr, src) =
  if important_constr constr then
    let open Grid in
    begin
      fprintf fmt "%d -> %d [%sfontsize=9] @[<h>// %a@]@;" src dest
        (if options.strong_deps_labels then
           let (Constr (x, _v)) = constr in
           asprintf "label=\"%a\", "
             (print_var env) (Var x)
         else "")
        (print_constr env "=") constr
    end

let event_time te i = 
  let open Trace in 
  let open Simulation_info in
  match Trace_explorer.step i te with
  | Subs _ -> 0.0
  | Rule (_, _, infos) -> infos.story_time
  | Init _ -> 0.0
  | Obs (_, _, infos) -> infos.story_time
  | Pert (_, _, infos) -> infos.story_time
  | Dummy _ -> 0.0
  

let print ?(options=def_options_simple) te fmt (evs, prec) =

  let choose_color =
    match options.nodes_coloring with
    | Build_order ->
      let n = List.length evs in
      fun _ cc_info -> 
        let i = cc_info.Causal_core.number_added in
        (0., 0., 1.0 -. 0.5 *. float_of_int (n - i) /. float_of_int n)
    | Time ->
      let maxT = list_maximum 
          (List.map (fun (i, _) -> event_time te i) evs) in
      fun i _ ->
        let t = event_time te i in
        (0., 0., (1.0 -. 0.4 *. t /. maxT)) in
        
  let pr x = Format.fprintf fmt x in
  let env = Trace_explorer.model te in

  pr "@[<v 2>digraph G{@;" ;
  pr "rankdir=\"TB\";@;" ;
  pr "ranksep=%.2f;@;" options.ranksep ;
  pr "node [fontname=\"%s\"];@;" options.font ;
  pr "edge [fontname=\"%s\"];@;" options.font ;
  pr "@;" ;
  evs  |> List.iter (print_event options te choose_color fmt) ;
  pr "@;" ;
  prec |> List.iter (print_prec_arrow options fmt) ;
  pr "@;" ;

  if options.show_strong_deps then
    begin
      let deps = Precedence.compute_strong_deps te (Causal_core.core_events evs) in
      (*print_int (List.length deps) ;
      print_newline () ; *)
      deps |> List.iter (print_strong_dep_arrow options env fmt)
    end ;

  pr "}@]@."
