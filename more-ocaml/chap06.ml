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

let rec rle max_len l =
  let rec rle_aux n a = function
    | [] -> []
    | [x] -> (n,x)::a
    | x::(y::_ as t) -> 
       if x = y && n < max_len
       then rle_aux (n+1) a t 
       else rle_aux 1 ((n,x)::a) t
  in
  List.rev (rle_aux 1 [] l)

(* Each run shall consist of a length byte followed by 1 to 128 bytes
of data. If the length byte is 0-127, the following length+1 bytes are
literal. If the length is 129-255, the following single byte shall be
copied 257-length (2-128) times. Length of 128 denotes EOD. *)

let compress s =
  let buf = Buffer.create 32 in
  let rec encode_lits count acc = function
    | (n,x)::xs when n = 1 -> encode_lits (count+1) (x::acc) xs
    | l when count = 0 -> encode_run l
    | l ->
      let len = char_of_int (count - 1) in
      Buffer.add_char buf len;
      List.iter (fun c -> Buffer.add_char buf c) (List.rev acc);
      encode_run l
  and encode_run = function
    | (n,x)::xs when n > 1 ->
      let len = char_of_int (257-n) in
      Buffer.add_char buf len;
      Buffer.add_char buf x;
      encode_lits 0 [] xs
    | _::_ as l -> encode_lits 0 [] l
    | [] -> ()
  in
  explode s |> rle 127 |> encode_run;
  Buffer.add_char buf '\128';
  Buffer.contents buf

let rec decode = function
  | ['\128'] -> []
  | x::xs when '\000' <= x && x <= '\127' ->
     let len = (int_of_char x) + 1 in
     let lits = take len xs in
     lits @ decode (drop len xs)
  | x::d::xs when '\129' <= x && x <= '\255' ->
     let n = 257 - (int_of_char x) in
     let reps = replicate n d in
     reps @ decode xs
  | [] -> failwith "expected EOD"
  | _ -> failwith "mangled data"

let expand s =
  explode s |> decode |> implode
