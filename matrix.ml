type t = { rows : int; cols : int; cells : float array }

let size m = (m.rows,m.cols)

(* Note: 1-based representation *)
let offset m r c = 
  (pred c) * m.rows + (pred r)

let get m r c = 
  m.cells.(offset m r c)

let set m r c v = 
  m.cells.(offset m r c) <- v

let make rows cols = 
  { rows; cols; cells = Array.make_float (rows*cols) }

let init rows cols f =
  let m = make rows cols in
  for i = 1 to rows do
    for j = 1 to cols do
      set m i j (f i j)
    done
  done; 
  m

let init_random rows cols =
  init rows cols (fun _ _ -> Random.float 1.0)

let init_identity rows cols =
  init rows cols (fun i j -> if i = j then 1.0 else 0.0)

let init_diag v =
  failwith "not implemented"

let row m r =
  init 1 m.cols (fun _ j -> get m r j)

let col m c = 
  init m.rows 1 (fun i _ -> get m i c)

let diag m =
  init (min m.rows m.cols) 1 (fun _ j -> get m j j)

let reshape m rows cols = 
  if m.rows * m.cols <> rows * cols then invalid_arg "Matrix.reshape";
  { rows; cols; cells = m.cells }

let transpose m = 
  init m.cols m.rows (fun i j -> get m j i)

let dot m n = failwith "not implemented"
let inv m = failwith "not implemented"
let eig m = failwith "not implemented"

let map f m =
  let rows = m.rows in
  let cols = m.cols in
  let cells = Array.init (rows*cols) (fun i -> f m.cells.(i)) in
  { rows; cols; cells }

let map2 f m1 m2 =
  if m1.rows <> m2.rows then invalid_arg "Matrix.map2";
  if m1.cols <> m2.cols then invalid_arg "Matrix.map2";
  let rows = m1.rows in
  let cols = m1.cols in
  let cs1 = m1.cells in
  let cs2 = m2.cells in
  let cells = Array.init (rows*cols) (fun i -> f cs1.(i) cs2.(i)) in
  { rows; cols; cells }

let neg m = 
  map (~-.) m

let add m1 m2 = 
  map2 (+.) m1 m2

let mult m1 m2 =
  if m1.cols <> m2.rows then invalid_arg "m2";
  let p = m1.cols in
  init m1.rows m2.cols
       begin 
         fun i j -> 
         let s = ref 0. in
         for k = 1 to p do
           s := !s +. (get m1 i k) *. (get m2 k j)
         done;
         !s
       end

let chain_mult ms =
  failwith "not implemented"

let from_list l =
  { rows = 1; cols = List.length l; cells = Array.of_list l }

let to_list m =
  Array.to_list m.cells

let from_string s =
  let open Str in
  let rows = split (regexp ";") s in
  let cols = List.map (split (regexp ",")) rows in
  let to_f = fun s -> float_of_string (String.trim s) in
  List.map (List.map to_f) cols;
  failwith "not implemented"

let to_string m =
  let open Printf in
  let b = Buffer.create (m.rows * m.cols * 10) in
  for i = 1 to m.rows do
    for j = 1 to m.cols do
      let e = get m i j in
      bprintf b "%0.2f" e;
      if j <> m.cols then bprintf b ", "
    done;
    bprintf b "\n"
  done;
  Buffer.contents b
