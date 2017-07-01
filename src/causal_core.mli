open Causal_core_shared
open Grid

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


type var_info = {
  modified_in_t : unit History.t ;
  set_to_def_in_t : unit History.t ;
  last_tested_to_def_in_sigma : step_id ;
  potential_closing_deps : step_id list
}

type var_info_table = (var', var_info) Hashtbl.t

val var_infos_of_grid : Model.t -> Grid.t -> step_id -> var_info_table

val causal_core_of_eois : Model.t -> Grid.t -> var_info_table -> step_id list -> causal_core
