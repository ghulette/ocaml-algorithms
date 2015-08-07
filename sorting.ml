module Naive = struct
    let merge l1 l2 =
      let rec insert x = function
        | [] -> [x]
        | y::_ as l when x <= y -> x::l
        | y::ys -> y :: insert x ys
      in
      List.fold_right insert l1 l2

    let rec split = function
      | [] -> [], []
      | [x] -> [x], []
      | x::y::l ->
         let r1, r2 = split l in
         x::r1, y::r2

    let rec merge_sort = function
      | [] -> []
      | [x] -> [x]
      | l ->
         let l1, l2 = split l in
         merge (merge_sort l1) (merge_sort l2)
  end

module Better = struct
    let merge l1 l2 =
      let rec aux acc = function
        | [], [] -> List.rev acc
        | x::xs, [] -> aux (x::acc) (xs, [])
        | [], y::ys -> aux (y::acc) ([], ys)
        | x::xs, y::ys ->
           if x < y then
             aux (y::x::acc) (xs, ys)
           else
             aux (x::y::acc) (xs, ys)
      in
      aux [] (l1, l2)

    let split l =
      let rec aux acc1 acc2 = function
        | [] -> List.rev acc1, List.rev acc2
        | [x] -> List.rev (x::acc1), List.rev acc2
        | x::y::l -> aux (x::acc1) (y::acc2) l
      in
      aux [] [] l

    let rec merge_sort = function
      | [] -> []
      | [x] -> [x]
      | l ->
         let l1, l2 = split l in
         merge (merge_sort l1) (merge_sort l2)
  end


let rec rands n =
  if n <= 0 then [] else (Random.int 100) :: (rands (n-1))

let time lbl f x =
  let t0 = Sys.time () in
  let fx = f x in
  let t1 = Sys.time () in
  Format.printf "%s: %fs\n" lbl (t1 -. t0);
  fx

let _ =
  Random.self_init ();
  let data = rands 10000 in
  time "naive merge_sort 10000" Naive.merge_sort data |> ignore;
  time "better merge_sort 10000" Better.merge_sort data
