module Make (Node : Set.OrderedType) = struct

  module S = Set.Make(Node)

  module M = Map.Make(Node)

  type node = Node.t

  type t = {
    nodes : S.t ;
    edges : S.t M.t }

  let empty = { nodes = S.empty ; edges = M.empty }

  let add_node n g =
    { g with nodes = S.add n g.nodes }

  let direct_successors x g =
    try M.find x g.edges with Not_found -> S.empty

  let add_edge (x, y) g =
    let g = add_node x (add_node y g) in
    {g with edges = M.add x (S.add y (direct_successors x g)) g.edges}

  let remove_edge (x, y) g =
    {g with edges = M.add x (S.remove y (direct_successors x g)) g.edges}

  let direct_successors_list x g = S.elements (direct_successors x g)

  let from_edges_list l = List.fold_right add_edge l empty

  let to_edges_list g =
    M.fold (fun x succ_x acc ->
        List.map (fun y -> (x, y)) (S.elements succ_x) :: acc) g.edges []
    |> List.concat


  (* Gives the set of reachable nodes from [n] *)
  let dfs n g =
    let rec aux opened visited =
      match opened with
      | [] -> visited
      | n :: ns ->
        if S.mem n visited then aux ns visited
        else
          aux (direct_successors_list n g @ opened) (S.add n visited) in
    aux [n] S.empty


  let transitive_reduction g =
    let process_edge (u, v) g =
      S.fold (fun v' g ->
          if v' <> v then remove_edge (u, v') g else g)
        (dfs v g) g in
    List.fold_right process_edge (to_edges_list g) g

end
