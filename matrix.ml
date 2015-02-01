module Matrix : sig
  type t
  val make : int -> int -> t
  val init : int -> int -> (int -> int -> float) -> t
  val make_random : int -> int -> t
  val make_identity : int -> int -> t
  val size : t -> int * int
  val reshape : t -> int -> int -> t
  val get : t -> int -> int -> float
  val row : t -> int -> t
  val col : t -> int -> t
  val transpose : t -> t
  val add : t -> t -> t
  val dot : t -> t -> float
  val mult : t -> t -> t
  val neg : t -> t
  val inv : t -> t
  val map : (float -> float) -> t -> t
  val fold_rows : (float -> float) -> t -> t
  val fold_cols : (float -> float) -> t -> t
  val from_list : float list -> t
  val to_list : t -> float list
  val from_string : string -> t
  val to_string : t -> string
end = struct
  type t = { rows : int;
             cols : int;
             elts : float array
           }

  (* Note: column-major representation, 0-based representation *)
  let offset m r c = (pred c) * m.rows + (pred r)

  let get m r c = m.elts.(offset m r c)
  let set m r c v = m.elts.(offset m r c) <- v
  let size m = (m.rows,m.cols)

  let reshape m rows cols = failwith "not implemented"
  let row m r = failwith "not implemented"
  let col m c = failwith "not implemented"
  let from_list l = failwith "not implemented"
  let to_list m = failwith "not implemented"

  let transpose m = failwith "not implemented"
  let dot m n = failwith "not implemented"
  let inv m = failwith "not implemented"

  let make_with_elts rows cols elts = { rows; cols; elts }

  let make rows cols = 
    let elts = Array.make_float (rows*cols) in
    make_with_elts rows cols elts

  let init rows cols f =
    let m = make rows cols in
    for i = 1 to rows do
      for j = 1 to cols do
        set m i j (f i j)
      done
    done; 
    m

  let make_random rows cols =
    let elts = Array.init (rows*cols) (fun _ -> Random.float 1.0) in
    make_with_elts rows cols elts

  let make_identity rows cols =
    let m = make rows cols in
    Array.fill m.elts 0 (rows*cols) 0.0;
    for i = 1 to min rows cols do
      set m i i 1.
    done;
    m

  let map f m =
    let rows = m.rows in
    let cols = m.cols in
    let elts = Array.init (rows*cols) (fun i -> f m.elts.(i)) in
    make_with_elts rows cols elts

  let fold_rows f m = failwith "not implemented"
  let fold_cols f m = failwith "not implemented"

  let neg m = map (~-.) m

  let add m1 m2 =
    let rows = m1.rows in
    let cols = m1.cols in
    let e1 = m1.elts in
    let e2 = m2.elts in
    let elts = Array.init (rows*cols) (fun i -> e1.(i) +. e2.(i)) in
    make_with_elts rows cols elts

  let mult m1 m2 =
    if m1.cols <> m2.rows then invalid_arg "m2" else
      let rows = m1.rows in
      let cols = m2.cols in
      let eltf i j = 0.0 in
      init rows cols eltf

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
    
  let from_string m = failwith "not implemented"
      
end
