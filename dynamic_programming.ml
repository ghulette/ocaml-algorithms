open Printf

let rec upto i j =
  if i > j then [] else i :: upto (i + 1) j

let list_init n f = 
  let rec aux = function
    | 0 -> []
    | n -> f n :: aux (n-1)
  in
  List.rev (aux n)

let random_costf n = 
  let r = Array.init n (fun _ -> Random.int 100) in
  function 0 -> 0 | x -> r.(x - 1)

(* Simple recursive definition *)
let rec cut_rod p n =
  let vs = list_init n (fun i -> p i + cut_rod p (n-i)) in
  List.fold_left max 0 vs

(* Memoized *)
let memoized_cut_rod p n =
  let r = Array.make (n+1) (-1) in
  let rec aux n =
    if r.(n) >= 0 then r.(n) else
      let vs = list_init n (fun i -> p i + aux (n-i)) in
      let q = List.fold_left max 0 vs in
      r.(n) <- q;
      q
  in 
  aux n

(* Bottom up *)
let bottom_up_cut_rod p n =
  let r = Array.make (n+1) (-1) in
  r.(0) <- 0;
  for j = 1 to n do
    let vs = list_init j (fun i -> (p i) + r.(j-i)) in
    r.(j) <- List.fold_left max 0 vs
  done;
  r.(n)

let _ =
  let n = 10 in
  Random.init 270113;
  let p = random_costf n in
  for i = 1 to n do
    let t0 = Sys.time () in
    let v = bottom_up_cut_rod p i in
    let t1 = Sys.time () in
    printf "%d: %d (%0.5f)\n" i v (t1-.t0);
    flush_all ()
  done
