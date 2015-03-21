type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec lseq n =
  Cons (n, fun () -> lseq (n + 1))

let lhd (Cons (x,_)) = x

let ltl (Cons (_,tf)) = tf ()

let rec ltake n (Cons (x,tf)) =
  if n = 0 then [] else x :: ltake (n-1) (tf ())

let rec ldrop n (Cons (_,tf) as l) =
  if n = 0 then l else ldrop (n-1) (tf ())

let rec lmap f (Cons (x,tf)) =
  Cons (f x, fun () -> lmap f (tf ()))

let rec lfilter p (Cons (x,tf)) =
  if p x then
    Cons (x, fun () -> lfilter p (tf ()))
  else
    lfilter p (tf ())

let cubes_divisible_by_5 =
  let cubes = lmap (fun x -> x * x * x) (lseq 1) in
  lfilter (fun x -> x mod 5 = 0) cubes

let primes = 
  let rec mkprimes (Cons (x,tf)) =
    Cons (x, fun () -> mkprimes (lfilter (fun y -> y mod x <> 0) (tf ())))
  in
  mkprimes (lseq 2)

let rec interleave l1 l2 =
  let Cons (x,tf) = l1 in
  Cons (x, fun () -> interleave l2 (tf ()))

let rec lconst k =
  Cons (k, fun () -> lconst k)

let interleaved_one_zero =
  interleave (lconst 0) (lconst 1)

let all_ones =
  let rec all_from l =
    Cons (l, fun () -> interleave (all_from (0::l)) (all_from (1::l)))
  in
  all_from []

(* Q1 *)
let powers_of_two =
  lmap (fun n -> 1 lsl n) (lseq 0)

(* Q2 *)
let rec lnth n (Cons (x,tf)) =
  if n = 0 then x else lnth (n-1) (tf ())

(* Q3 *)
let cycle l =
  let rec cycle_aux m = function
    | [] -> invalid_arg "cycle"
    | [x] -> Cons (x, fun () -> cycle_aux m m)
    | x::xs -> Cons (x, fun () -> cycle_aux m xs)
  in
  cycle_aux l l

(* Q4 *)
let fibs =
  let rec fibs_aux a b = 
    Cons (a, fun () -> fibs_aux b (a + b))
  in
  fibs_aux 0 1

(* Q5 *)
let rec unleave l = 
  let Cons (x,tf) = l in
  let Cons (y,tf') = tf () in
  let l1 = Cons (x, fun () -> fst (unleave (tf' ()))) in
  let l2 = Cons (y, fun () -> snd (unleave (tf' ()))) in
  (l1,l2)

(* Q6 *)
let labels =
  let rec label n =
    if n <= 26 then
      String.make 1 (char_of_int (n + 64))
    else
      label ((n-1)/26) ^ label (((n-1) mod 26)+1)
  in
  lmap label (lseq 1)

let rec map2 f l1 l2 =
  let Cons (x,xtf) = l1 in
  let Cons (y,ytf) = l2 in
  Cons (f x y, fun () -> map2 f (xtf ()) (ytf ()))

    
