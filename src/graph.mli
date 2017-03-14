module Make : functor (Node : Set.OrderedType) ->
sig
  module S : (Set.S with type elt = Node.t)
  module M : (Map.S with type key = Node.t)

  type t
  type node = Node.t

  val empty : t
  val add_node : node -> t -> t
  val direct_successors : node -> t -> S.t
  val add_edge : node * node -> t -> t
  val remove_edge : node * node -> t -> t
  val direct_successors_list : node -> t -> node list
  val from_edges_list : (node * node) list -> t
  val to_edges_list : t -> (node * node) list
  val dfs : node -> t -> S.t
  val transitive_reduction : t -> t
end
