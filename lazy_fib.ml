type 'a lazy_list = Cons of 'a * ('a lazy_list Lazy.t)

let rec zip_with f (Cons (x,lazy xs)) (Cons (y,lazy ys)) =
  Cons (f x y,lazy (zip_with f xs ys))

let rec take n (Cons (x,lazy xs)) =
  if n = 0 then [] else x :: take (n-1) xs

let rec ones = Cons (1,lazy ones)

let twos = zip_with (+) ones ones

let rec map f (Cons (x,lazy xs)) =
  Cons (f x, lazy (map f xs))

let rec filter p (Cons (x,lazy xs)) =
  if p x then Cons (x, lazy (filter p xs)) else filter p xs

(* Nope *)
let rec fib = 
  let l1 = Cons (1,lazy (Lazy.force fib)) in
  let l2 = Cons (1,lazy (Cons (1,lazy (Lazy.force fib)))) in
  lazy (zip_with (+) l1 l2)
