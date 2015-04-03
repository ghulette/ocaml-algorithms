open Io

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

let print_packed w s =
  let i = BitInput.of_input (Input.of_string s) in
  try
    while true do
      for col = 1 to w do
        print_int (BitInput.read_bit i |> fun b -> if b then 1 else 0)
      done;
      print_newline ()
    done
  with
    End_of_file -> ()

let _ =
  pack Bitmap.data |> print_packed Bitmap.width
