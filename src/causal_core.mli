open Causal_core_shared

type sigma_event_info = { number_added : int }

type causal_core = (step_id * sigma_event_info) list

type t = causal_core

val iter_causal_cores :
  Model.t ->
  Grid.t ->
  step_id list ->
  (step_id -> causal_core -> unit) ->
  unit

val core_events : causal_core -> step_id list