module List : sig
  include (module type of List)
  val from : int -> int -> int list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
end
