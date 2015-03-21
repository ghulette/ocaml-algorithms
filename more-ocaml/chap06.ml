let rec take n l =
  if n = 0 then [] else (List.hd l) :: take (n-1) (List.tl l)

let rec take_while p = function
  | x::xs when p x -> x :: (take_while p xs)
  | _ -> []

let rec drop n l =
  if n = 0 then l else drop (n-1) (List.tl l)

let rec replicate n x =
  if n = 0 then [] else x :: replicate (n-1) x

let implode l =
  let b = Buffer.create (List.length l) in
  List.iter (fun c -> Buffer.add_char b c) l;
  Buffer.contents b

let explode s =
  let cs = ref [] in
  String.iter (fun c -> cs := c :: !cs) s;
  List.rev !cs

let string_of_int_list l =
  List.map char_of_int l |> implode

let int_list_of_string s =
  explode s |> List.map int_of_char

(* Each run shall consist of a length byte followed by 1 to 128 bytes
of data. If the length byte is 0-127, the following length+1 bytes are
literal. If the length is 129-255, the following single byte shall be
copied 257-length (2-128) times. Length of 128 denotes EOD. *)

let rec decode = function
  | [] -> failwith "expected EOD"
  | [128] -> []
  | x::xs when 0 <= x && x <= 127 ->
     let len = x + 1 in
     let lits = take len xs in
     lits @ decode (drop len xs)
  | x::d::xs when 129 <= x && x <= 255 ->
     let n = 257 - x in
     let reps = replicate n d in
     reps @ decode xs
  | _ -> failwith "mangled data"

let rle max_run = function 
  | [] -> []
  | c::cs -> 
     let f = fun a x' -> 
       match a with 
       | (x,n)::l -> if x = x' && n <= max_run
                     then (x,n+1)::l 
                     else (x',1)::(x,n)::l
       | _ -> assert false
     in
     List.rev (List.fold_left f [(c,1)] cs)

let encode l = 
  let rec encode_aux a = function
    | [] -> 
       let len = List.length a in
       if len > 0 then (len-1) :: (List.rev a) else []
    | (x,n)::rs when n = 1 -> encode_aux (x::a) rs
    | (x,n)::rs when n > 1 -> 
       let e = encode_aux a [] in
       e @ [257-n;x] @ (encode_aux [] rs)
    | _ -> assert false
  in
  encode_aux [] (rle 127 l) @ [128]
   
let compress s =
  int_list_of_string s |> encode |> string_of_int_list

let expand s =
  int_list_of_string s |> decode |> string_of_int_list
