open Causal_core_shared

type t

val load_from_file : string -> t

val model : t -> Model.t

val step : step_id -> t -> Trace.step

val last_step_id : t -> step_id

val fold : 
  init:'a -> (int -> Trace.step -> 'a -> 'a) -> t -> 'a


module Grid :
sig

  val build : t -> unit

  val tests : step_id -> t -> Grid.constr list

  val actions : step_id -> t -> Grid.constr list

end