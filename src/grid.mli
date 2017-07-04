(** "Musical notation" representation of traces. *)

type concrete_agent = Instantiation.concrete
type concrete_site  = concrete_agent Instantiation.site
type internal_state = Instantiation.internal_state

type concrete_binding_state = Free | Bound of concrete_site


(** {6 Variables and constraints} *)

type 'a var =
  | Internal_state  : concrete_site  -> internal_state var
  | Binding_state   : concrete_site  -> concrete_binding_state var
  | Agent_existence : concrete_agent -> bool var

val default : Model.t -> 'a var -> 'a

type var' = Var : 'a var -> var'

type constr = Constr : 'a var * 'a -> constr


(** {6 Grid generation utilities} *)

val translate_actions : 
  concrete_agent Instantiation.action list -> constr list

val translate_tests :
  query_state:(var' -> constr option) ->
  concrete_agent Instantiation.test list -> constr list


(** {6 Pretty printing} *)

val print_tests : 
  Model.t -> Format.formatter -> constr list -> unit

val print_actions :
  Model.t -> Format.formatter -> constr list -> unit

val print_constr : 
  Model.t -> string -> Format.formatter -> constr -> unit

val print_var :
  Model.t ->  Format.formatter -> var' -> unit