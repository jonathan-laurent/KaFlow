let rec flatten_list_option = function
  | [] -> []
  | None :: l -> flatten_list_option l
  | Some x :: l -> x :: flatten_list_option l

let list_maximum l = List.fold_left max (List.hd l) l

let list_minimum l = List.fold_left max (List.hd l) l

let rec replicate n x =
  if n = 0 then []
  else x :: replicate (n-1) x

let rec range_list a b =
  if b <= a then []
  else a :: range_list (a+1) b

let random_int_list ~size max =
  List.map (fun () -> Random.int max) (replicate size ())

let rec list_take n = function
  | [] -> []
  | x::xs -> if n <= 0 then [] else x :: list_take (n-1) xs

module Int = struct
  type t = int
  let compare = compare
end

let list_of_queue q = List.rev (Queue.fold (fun acc x -> x :: acc) [] q)

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (Model.get_rule env rule_id).Primitives.syntactic_rule