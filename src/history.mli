(* Sets of integers that are optimized to answer requests of the form:
   "what is the last element in t whose index is strictly greater than i" *)

  type t

  val from_queue : int Queue.t -> t

  val first_after : int -> t -> int option
  
  val last_before : int -> t -> int option