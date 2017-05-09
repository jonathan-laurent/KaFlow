open Causal_core_shared

type concrete_agent = Instantiation.concrete
type concrete_site  = Instantiation.concrete Instantiation.site
type internal_state = Instantiation.internal_state

type concrete_binding_state = Free | Bound of concrete_site
 
type 'a var =
  | Internal_state  : concrete_site  -> internal_state var
  | Binding_state   : concrete_site  -> concrete_binding_state var
  | Agent_existence : concrete_agent -> bool var

val default : Model.t -> 'a var -> 'a

type var' = Var : 'a var -> var'
type constr = Constr : 'a var * 'a -> constr
type constrs = constr list

type grid = (constrs * constrs) array
type t = grid

val build_grid : ?rule:(string option) -> Model.t -> Trace.t -> t * step_id list

val symmetrize_binding_constrs : t -> t

val print_tests : 
  Model.t -> Format.formatter -> constrs -> unit

val print_actions :
  Model.t -> Format.formatter -> constrs -> unit

val print_constr : 
  Model.t -> string -> Format.formatter -> constr -> unit

val print_var :
  Model.t ->  Format.formatter -> var' -> unit
