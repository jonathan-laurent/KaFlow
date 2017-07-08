open Causal_core_shared

val compute_precedence :
  Trace_explorer.t -> step_id list ->
  (step_id * step_id) list

val transitive_reduction :
  (step_id * step_id) list ->
  (step_id * step_id) list

val compute_strong_deps :
    ?compute_all_activations:bool ->
    Trace_explorer.t -> step_id list ->
    (step_id * Grid.constr * step_id) list