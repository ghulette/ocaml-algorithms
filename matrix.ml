module type MATRIX = sig
  type t
  val make : int -> int -> t
  val init : int -> int -> (int -> int -> float) -> t
  val init_random : int -> int -> t
  val init_identity : int -> int -> t
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
end 
module Matrix = struct
  type t = { rows : int; cols : int; cells : float array }

  (* Note: column-major representation, 0-based representation *)
  let offset m r c = (pred c) * m.rows + (pred r)

  let get m r c = m.cells.(offset m r c)
  let set m r c v = m.cells.(offset m r c) <- v
  let size m = (m.rows,m.cols)

  let reshape m rows cols = failwith "not implemented"
  let row m r = failwith "not implemented"
  let col m c = failwith "not implemented"
  let from_list l = failwith "not implemented"
  let to_list m = failwith "not implemented"

  let transpose m = failwith "not implemented"
  let dot m n = failwith "not implemented"
  let inv m = failwith "not implemented"

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

  let map f m =
    let rows = m.rows in
    let cols = m.cols in
    let cells = Array.init (rows*cols) (fun i -> f m.cells.(i)) in
    { rows; cols; cells }

  let fold_rows f m = failwith "not implemented"
  let fold_cols f m = failwith "not implemented"

  let neg m = map (~-.) m

  let add m1 m2 =
    let rows = m1.rows in
    let cols = m1.cols in
    let e1 = m1.cells in
    let e2 = m2.cells in
    let cells = Array.init (rows*cols) (fun i -> e1.(i) +. e2.(i)) in
    { rows; cols; cells }

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

  module Parser = struct
    let rec parse s =
      let b = Buffer.create 16 in
      let row = ref [] in
      let rows = ref [] in
      String.iter 
        begin function
          | '\n' | '\t' | ' ' -> ()
          | ';' -> 
             let n = float_of_string (Buffer.contents b) in
             Buffer.clear b;
             row := n :: !row;
             rows := (List.rev !row) :: !rows;
             row := []
          | ',' ->
             let n = float_of_string (Buffer.contents b) in
             Buffer.clear b;
             row := n :: !row
          | c -> Buffer.add_char b c
        end s;
      if Buffer.length b <> 0 then 
        begin
          let n = float_of_string (Buffer.contents b) in
          row := n :: !row
        end;
      if List.length !row <> 0 then
        begin
          rows := !row :: !rows;
        end;
      List.rev !rows
  end
    
  let from_string s =
    from_list (Parser.parse s)
      
end
