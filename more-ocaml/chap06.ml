open Io

let bit_of_bool b =
  if b then 1 else 0

let string_of_int_list l =
  let bs = Bytes.create (List.length l) in
  List.iteri (Bytes.set bs) (List.map char_of_int l);
  Bytes.to_string bs

let int_list_of_string s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := int_of_char s.[i] :: !l
  done;
  !l

let process f s =
  let b = Buffer.create (String.length s) in
  f (Input.of_string s) (Output.of_buffer b);
  Buffer.contents b

exception EOD

let decompress i o =
  try
    while true do
      match int_of_char (Input.read_char i) with
      | x when 0 <= x && x <= 127 ->
         for p = 1 to x + 1 do 
           Input.read_char i |> Output.write_char o 
         done
      | x when 128 < x && x <= 255 ->
         let c = Input.read_char i in
         for p = 1 to 257 - x do Output.write_char o c done
      | _ -> raise EOD
    done
  with
    EOD -> ()

let decompress_string = process decompress

let get_same i =
  let rec get_count ch c =
    if c = 128 then 128 else
      try
        if Input.read_char i = ch
        then get_count ch (c + 1)
        else (Input.rewind i; c)
      with 
        End_of_file -> c
  in
  let ch = Input.read_char i in (ch, get_count ch 1)

let get_different i =
  let rec get_diff_aux a c =
    if c = 128 then List.rev a else
      try
        let ch' = Input.read_char i in
        if ch' <> List.hd a
        then get_diff_aux (ch'::a) (c+1)
        else (Input.rewind i; Input.rewind i; List.rev (List.tl a))
      with
        End_of_file -> List.rev a
  in
  get_diff_aux [Input.read_char i] 1

let compress i o =
  try
    while true do
      match get_same i with
      | (_,1) ->
         Input.rewind i;
         let cs = get_different i in
         Output.write_char o (char_of_int (List.length cs - 1));
         List.iter (Output.write_char o) cs
      | (b,c) ->
         Output.write_char o (char_of_int (257 - c));
         Output.write_char o b
    done
  with
    End_of_file -> Output.write_char o (char_of_int 128)

let compress_string = process compress

let _ =
  let open Printf in
  let src = "((5.000000, 4.583333), (4.500000, 5.000000))" in
  let enc = compress_string src in
  let dec = decompress_string enc in
  let ratio = 
    let src_len = String.length src |> float_of_int in
    let enc_len = String.length enc |> float_of_int in
    enc_len /. src_len
  in
  printf "Original:     %s\n" src;
  printf "Decompressed: %s\n" dec;
  printf "Compression:  %0.3f\n" ratio;
  if src = dec then printf "Ok\n" else failwith "Compression error!"


module Bitmap = struct
  let width = 80
  let height = 21

  let data =
    "00000000000000000000000000000000000000000000000000000000000000000000000000000000\
     00000000000000000000000000000000000000000000000000000000000000000000000001000000\
     00000000111111110000000000011111111100000000000000000000000000000000000111100000\
     00000011000000011100000001110000001110000000000000000000000000000000000011000000\
     00000110000000001110000011000000000110000000000000000000000000000000000011000000\
     00001110000000000111000111000000000000000000000000000000000000000000000011000000\
     00001100000000000111000110000000000000000000000000000000000000000000000011000000\
     00001100000000000011001110000000000000000011100000000100111000011100000011000000\
     00011100000000000011001110000000000000001111111000111111111101111110000001000000\
     00011100000000000011101100000000000000001000011000001110001111000111000001000000\
     00011100000000000011101100000000000000000000011000001100000110000011000001000000\
     00011100000000000011001110000000000000000000011000001100000110000011000001000000\
     00001100000000000011001110000000000000000111011000001100000110000011000001000000\
     00001110000000000011000110000000000000011100011000001100000110000011000001000000\
     00001110000000000110000111000000000000011000011000001100000110000011000011000000\
     00000111000000000110000011100000000000011000011000001100000110000011000011100000\
     00000011100000001100000001110000000010010001110000011000001100000110000111000000\
     00000011111111100000000001111111111000111110111000111100011110000111001111100000\
     00000000011100000000000000001111000000001000000000000000000000000000000000000000\
     00000000000000000000000000000000000000000000000000000000000000000000000000000000\
     00000000000000000000000000000000000000000000000000000000000000000000000000000000"
end

let pack s =
  let b = Buffer.create (String.length s / 8 + 1) in
  let o = BitOutput.of_output (Output.of_buffer b) in
  for x = 0 to String.length s - 1 do 
    BitOutput.write_bool o (s.[x] = '1')
  done;
  BitOutput.flush o;
  Buffer.contents b

let print_packed s w =
  let i = BitInput.of_input (Input.of_string s) in
  try
    while true do
      for col = 1 to w do
        let b = BitInput.read_bool i in
        print_int (if b then 1 else 0)
      done;
      print_newline ()
    done
  with
    End_of_file -> ()

let white_terminating_codes =
  [|[0; 0; 1; 1; 0; 1; 0; 1]; 
    [0; 0; 0; 1; 1; 1];
    [0; 1; 1; 1];
    [1; 0; 0; 0];
    [1; 0; 1; 1];
    [1; 1; 0; 0];
    [1; 1; 1; 0];
    [1; 1; 1; 1];
    [1; 0; 0; 1; 1];
    [1; 0; 1; 0; 0];
    [0; 0; 1; 1; 1];
    [0; 1; 0; 0; 0];
    [0; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1];
    [1; 1; 0; 1; 0; 0];
    [1; 1; 0; 1; 0; 1];
    [1; 0; 1; 0; 1; 0];
    [1; 0; 1; 0; 1; 1];
    [0; 1; 0; 0; 1; 1; 1];
    [0; 0; 0; 1; 1; 0; 0];
    [0; 0; 0; 1; 0; 0; 0];
    [0; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 1];
    [0; 0; 0; 0; 1; 0; 0];
    [0; 1; 0; 1; 0; 0; 0];
    [0; 1; 0; 1; 0; 1; 1];
    [0; 0; 1; 0; 0; 1; 1];
    [0; 1; 0; 0; 1; 0; 0];
    [0; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 1];
    [0; 0; 0; 1; 1; 0; 1; 0];
    [0; 0; 0; 1; 1; 0; 1; 1];
    [0; 0; 0; 1; 0; 0; 1; 0];
    [0; 0; 0; 1; 0; 0; 1; 1];
    [0; 0; 0; 1; 0; 1; 0; 0];
    [0; 0; 0; 1; 0; 1; 0; 1];
    [0; 0; 0; 1; 0; 1; 1; 0];
    [0; 0; 0; 1; 0; 1; 1; 1];
    [0; 0; 1; 0; 1; 0; 0; 0];
    [0; 0; 1; 0; 1; 0; 0; 1];
    [0; 0; 1; 0; 1; 0; 1; 0];
    [0; 0; 1; 0; 1; 0; 1; 1];
    [0; 0; 1; 0; 1; 1; 0; 0];
    [0; 0; 1; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1];
    [0; 0; 0; 0; 1; 0; 1; 0];
    [0; 0; 0; 0; 1; 0; 1; 1];
    [0; 1; 0; 1; 0; 0; 1; 0];
    [0; 1; 0; 1; 0; 0; 1; 1];
    [0; 1; 0; 1; 0; 1; 0; 0];
    [0; 1; 0; 1; 0; 1; 0; 1];
    [0; 0; 1; 0; 0; 1; 0; 0];
    [0; 0; 1; 0; 0; 1; 0; 1];
    [0; 1; 0; 1; 1; 0; 0; 0];
    [0; 1; 0; 1; 1; 0; 0; 1];
    [0; 1; 0; 1; 1; 0; 1; 0];
    [0; 1; 0; 1; 1; 0; 1; 1];
    [0; 1; 0; 0; 1; 0; 1; 0];
    [0; 1; 0; 0; 1; 0; 1; 1];
    [0; 0; 1; 1; 0; 0; 1; 0];
    [0; 0; 1; 1; 0; 0; 1; 1];
    [0; 0; 1; 1; 0; 1; 0; 0]|]

let black_terminating_codes =
  [|[0; 0; 0; 1; 1; 0; 1; 1; 1];
    [0; 1; 0];
    [1; 1];
    [1; 0];
    [0; 1; 1];
    [0; 0; 1; 1];
    [0; 0; 1; 0];
    [0; 0; 0; 1; 1];
    [0; 0; 0; 1; 0; 1];
    [0; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 1; 0; 1];
    [0; 0; 0; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1; 0];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1; 0];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 0; 1];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1; 0];
    [0; 0; 0; 0; 1; 1; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1; 0];
    [0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1; 1];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 0];
    [0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1; 1]|]

let white_make_up_codes =
  [|[1; 1; 0; 1; 1];
    [1; 0; 0; 1; 0];
    [0; 1; 0; 1; 1; 1];
    [0; 1; 1; 0; 1; 1; 1];
    [0; 0; 1; 1; 0; 1; 1; 0];
    [0; 0; 1; 1; 0; 1; 1; 1];
    [0; 1; 1; 0; 0; 1; 0; 0];
    [0; 1; 1; 0; 0; 1; 0; 1];
    [0; 1; 1; 0; 1; 0; 0; 0];
    [0; 1; 1; 0; 0; 1; 1; 1];
    [0; 1; 1; 0; 0; 1; 1; 0; 0];
    [0; 1; 1; 0; 0; 1; 1; 0; 1];
    [0; 1; 1; 0; 1; 0; 0; 1; 0];
    [0; 1; 1; 0; 1; 0; 0; 1; 1];
    [0; 1; 1; 0; 1; 0; 1; 0; 0];
    [0; 1; 1; 0; 1; 0; 1; 0; 1];
    [0; 1; 1; 0; 1; 0; 1; 1; 0];
    [0; 1; 1; 0; 1; 0; 1; 1; 1];
    [0; 1; 1; 0; 1; 1; 0; 0; 0];
    [0; 1; 1; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 1; 1; 0; 1; 0];
    [0; 1; 1; 0; 1; 1; 0; 1; 1];
    [0; 1; 0; 0; 1; 1; 0; 0; 0];
    [0; 1; 0; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 0; 0];
    [0; 1; 0; 0; 1; 1; 0; 1; 1]|]

let black_make_up_codes =
  [|[0; 0; 0; 0; 0; 0; 1; 1; 1; 1];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0; 1];
    [0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 0; 1; 0; 1];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 0; 1; 1; 0; 1; 1];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 1; 0; 1]|]

let rec code is_black len =
  if len > 1791 || len < 0 then invalid_arg "code";
  if len > 64 then
    let m = 
      if is_black
      then black_make_up_codes.(len / 64 - 1)
      else white_make_up_codes.(len / 64 - 1)
    in m @ code is_black (len mod 64)
  else
    if is_black
    then black_terminating_codes.(len)
    else white_terminating_codes.(len)

let read_up_to color ib w =
  let rec aux n =
    if n >= w then (n, color) else
      let next = BitInput.peek ib in
      if next = color then
        begin
          BitInput.read_bool ib |> ignore;
          aux (n+1)
        end
      else
        (n, color)
  in
  aux 0

let encode_fax ib ob w h =
  let open BitInput in
  let open BitOutput in
  let rec encode_fax_line w =
    if w > 0 then
      let color = peek ib in
      let n, is_black = read_up_to color ib w in
      List.iter (write_bit ob) (code color n);
      encode_fax_line (w - n)
  in
  for x = 1 to h do
    if peek ib then List.iter (write_bit ob) (code true 0);
    encode_fax_line w
  done

let process_bits f s w h =
  let buf = Buffer.create (String.length s) in
  let ib = BitInput.of_input (Input.of_string s) in
  let ob = BitOutput.of_output (Output.of_buffer buf) in
  f ib ob w h;
  BitOutput.flush ob;
  Buffer.contents buf

let compress_ccitt = 
  process_bits encode_fax

module Prefix = struct
  type 'a t =
    | Leaf of 'a option
    | Node of ('a t * 'a t)

  let empty =
    Leaf None

  let rec insert k v t =
    match k, t with
    | [], _ -> Leaf (Some v)
    | 0::bs, Node (lt, rt) -> Node (insert bs v lt, rt)
    | 0::bs, Leaf _ -> Node (insert bs v empty, empty)
    | 1::bs, Node (lt, rt) -> Node (lt, insert bs v rt)
    | 1::bs, Leaf _ -> Node (empty, insert bs v empty)
    | _ -> invalid_arg "insert"

  let rec lookup k t =
    match k, t with
    | [], Leaf x -> x
    | 0::bs, Node (lt,_) -> lookup bs lt
    | 1::bs, Node (_,rt) -> lookup bs rt
    | _ -> None

  let lookup_step b = function
    | Node (lt,rt) -> if b = 0 then lt else rt
    | _ -> failwith "lookup_step"

  let of_array a =
    let t = ref empty in
    Array.iteri (fun v k -> t := insert k v !t) a;
    !t

end

let make_code_tree terminating make_up =
  let t = ref Prefix.empty in
  Array.iteri (fun v k -> t := Prefix.insert k v !t) terminating;
  Array.iteri (fun v k -> t := Prefix.insert k ((v+1)*64) !t) make_up;
  !t

let white_prefix =
  make_code_tree white_terminating_codes white_make_up_codes

let black_prefix = 
  make_code_tree black_terminating_codes black_make_up_codes

let rec read_code pf t ib =
  let open Prefix in
  let b = BitInput.read_bool ib |> bit_of_bool in
  match lookup_step b t with
  | Leaf (Some x) -> if x < 64 then x else x + read_code pf pf ib
  | t' -> read_code pf t' ib

let read_code is_black =
  let pf = if is_black then black_prefix else white_prefix in
  read_code pf pf

let decode_fax ib ob w h =
  let lines = ref h in
  let pixels = ref w in
  let is_black = ref false in
  while !lines > 0 do
    while !pixels > 0 do
      let n = read_code !is_black ib in
      for x = 1 to n do
        BitOutput.write_bool ob !is_black
      done;
      pixels := !pixels - n;
      is_black := not !is_black
    done;
    is_black := false;
    pixels := w;
    lines := !lines - 1;
  done

let decompress_ccitt =
  process_bits decode_fax

let _ =
  let open Printf in
  let src = pack Bitmap.data in
  print_packed src Bitmap.width;
  let enc = compress_ccitt src Bitmap.width Bitmap.height in
  let olen = String.length src |> float_of_int in
  let elen = String.length enc |> float_of_int in
  let perc = elen /. olen *. 100.0 in
  printf "Compressed to %0.2f%% of original size\n" perc;
  let dec = decompress_ccitt enc Bitmap.width Bitmap.height in
  print_packed dec Bitmap.width;
  if src = dec then printf "Ok\n" else failwith "Compression error!"


(* Q3 *)
let _ =
  let open Printf in
  let src = pack Bitmap.data in

  (* Encoding one line does slightly better *)
  let enc = compress_ccitt src 1680 1 in
  let olen = String.length src |> float_of_int in
  let elen = String.length enc |> float_of_int in
  let perc = elen /. olen *. 100.0 in
  printf "One line: %0.2f%% of original size\n" perc;

  (* Re-encoding data blows up the size *)
  let w = String.length enc * 8 in
  let enc2 = compress_ccitt enc w 1 in
  let e2len = String.length enc2 |> float_of_int in
  let perc2 = e2len /. elen *. 100.0 in
  printf "Re-encode: %0.2f%% of original size\n" perc2


(* Q4 *)
let bit_runs ib =
  let rec aux color n acc =
    try
      let next = BitInput.read_bool ib in
      if next = color then
        aux color (n+1) acc
      else
        let run = (color, n) in
        aux (not color) 1 (run::acc)
    with
      End_of_file -> acc
  in
  let start_color = BitInput.peek ib in
  aux start_color 0 []

let histogram l =
  let module M = Map.Make (struct type t = int let compare = compare end) in
  let record k m =
    if M.mem k m
    then let n = M.find k m in M.add k (n+1) m
    else M.add k 1 m
  in
  List.fold_left (fun a x -> record x a) M.empty l |> M.bindings

let bitstogram ib =
  let bruns, wruns = bit_runs ib |> List.partition fst in
  (bruns |> List.map snd |> histogram, wruns |> List.map snd |> histogram)

let _ =
  let open Printf in
  let data = Bitmap.data |> pack in
  let ib = BitInput.of_input (Input.of_string data) in
  let bh, wh = bitstogram ib in
  printf "Histograms\n";
  printf "Black: ";
  List.iter (fun (k, v) -> printf "%d,%d " k v) bh;
  printf "\n";
  printf "White: ";
  List.iter (fun (k, v) -> printf "%d,%d " k v) wh;
  printf "\n"
