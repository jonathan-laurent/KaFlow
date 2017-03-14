open Causal_core_shared
open Causal_core

val compute_precedence :
  Trace.t -> Grid.t -> causal_core ->
  (step_id * step_id) list

val transitive_reduction :
  (step_id * step_id) list ->
  (step_id * step_id) list

val compute_strong_deps :
    Model.t -> Grid.t -> causal_core ->
    (step_id * Grid.constr * step_id) list
