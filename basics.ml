module List : sig
  include module type of List
  val take : int -> 'a list -> 'a list
  val take_while : ('a -> bool) -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val drop_while : ('a -> bool) -> 'a list -> 'a list
end = struct
  include List

  let take n xs =
    let rec take_aux l n xs =
      if n == 0 then List.rev l else
        match xs with
        | [] -> List.rev l
        | x::xs -> take_aux (x::l) (n-1) xs
    in
    take_aux [] n xs

  let take_while p xs =
    let rec take_while_aux l p = function
      | [] -> List.rev l
      | x::xs -> if p x then take_while_aux (x::l) p xs else List.rev l
    in
    take_while_aux [] p xs

  let rec drop n xs =
    match (n,xs) with
    | (0,_) -> xs
    | (_,[]) -> []
    | (n,_::xs') -> drop (n-1) xs'

  let rec drop_while p = function
    | [] -> []
    | x::xs -> if p x then drop_while p xs else xs
end

module Func : sig
  val id : 'a -> 'a
  val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
end = struct
  let id x = x
  let compose f g = fun x -> f (g x)
end
