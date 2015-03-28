open Printf
open Big_int

module type Nat = sig 
  type t
  val (+) : t -> t -> t
  val of_int : int -> t
  val to_string : t -> string
end

module IntNat : (Nat with type t = int) = struct
  type t = int
  let (+) = (+)
  let of_int i = i
  let to_string = string_of_int
end

module BigNat : (Nat with type t = big_int) = struct
  type t = big_int
  let (+) = add_big_int
  let of_int = big_int_of_int
  let to_string = string_of_big_int
end

module Fibonacci (M : Nat) = struct
  let rec fib n =
    let m = ref (M.of_int 1) in
    let v = ref (M.of_int 1) in
    for i = 2 to n do
      let m' = !v in
      v := M.(!v + !m);
      m := m';
    done;
    !v
end

let test n =
  let module F = Fibonacci (BigNat) in
  let open F in
  for i = 1 to n do
    let t0 = Sys.time () in
    let v = fib i in
    let t1 = Sys.time () in
    printf "%d: %s (%0.5f)\n" i (BigNat.to_string v) (t1-.t0);
    flush_all ()
  done

let _ =
  test 10

