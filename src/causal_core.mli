open Causal_core_shared

type sigma_event_info = { number_added : int }

type causal_core = (step_id * sigma_event_info) list

type t = causal_core

val core_events : causal_core -> step_id list


(* {6 Simple interface} *)

val iter_causal_cores :
  Trace_explorer.t ->
  step_id list ->
  (step_id -> causal_core -> unit) ->
  unit

(* {6 Expert interface} *)

type var_info_table

val init_var_infos :
  ?last_step_id:step_id -> Trace_explorer.t -> var_info_table

val compute_causal_core :
  Trace_explorer.t -> var_info_table -> step_id list -> t