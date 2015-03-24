let string_of_int_list l =
  let bs = Bytes.create (List.length l) in
  List.iteri (Bytes.set bs) (List.map char_of_int l);
  Bytes.to_string bs

let int_of_string_list s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := int_of_char s.[i] :: !l
  done;
  !l

let process f s =
  let b = Buffer.create (String.length s) in
  f (input_of_string s) (output_of_buffer b);
  Buffer.contents b

exception EOD

let decompress i o =
  try
    while true do
      match int_of_char (i.input_char ()) with
      | x when 0 <= x && x <= 127 ->
         for p = 1 to x + 1 do o.output_char (i.input_char ()) done
      | x when 128 < x && x <= 255 ->
         let c = i.input_char () in
         for p = 1 to 257 - x do o.output_char c done
      | _ -> raise EOD
    done
  with
    EOD -> ()

let decompress_string = process decompress

let get_same i =
  let rec get_count ch c =
    if c = 128 then 128 else
      try
        if i.input_char () = ch
        then get_count ch (c+1)
        else (rewind i; c)
      with 
        End_of_file -> c
  in
  let ch = i.input_char () in (ch, get_count ch 1)

let get_different i =
  let rec get_diff_aux a c =
    if c = 128 then List.rev a else
      try
        let ch' = i.input_char () in
        if ch' <> List.hd a
        then get_diff_aux (ch'::a) (c+1)
        else (rewind i; rewind i; List.rev (List.tl a))
      with
        End_of_file -> List.rev a
  in
  get_diff_aux [i.input_char ()] 1

let compress i o =
  try
    while true do
      match get_same i with
      | (_,1) ->
         rewind i;
         let cs = get_different i in
         o.output_char (char_of_int (List.length cs - 1));
         List.iter o.output_char cs
      | (b,c) ->
         o.output_char (char_of_int (257 - c));
         o.output_char b
    done
  with
    End_of_file -> o.output_char (char_of_int 128)

let compress_string = process compress

let _ =
  let ex1 = "((5.000000, 4.583333), (4.500000, 5.000000))" in
  if compress_string ex1 |> decompress_string = ex1
  then printf "Ok\n"
  else printf "No!\n"

let width = 80

let bitmap_data =
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

let packedstring_of_string s =
  let b = Buffer.create (String.length s / 8 + 1) in
  let o = output_bits_of_output (output_of_buffer b) in
  for x = 0 to String.length s - 1 do put_bool_val o (s.[x] = '1') done;
  flush o;
  Buffer.contents b

let print_packedstring w s =
  let ibits = input_bits_of_input (input_of_string s) in
  try
    while true do
      for col = 1 to w do
        print_int (get_bit ibits |> fun b -> if b then 1 else 0)
      done;
      print_newline ()
    done
  with
    End_of_file -> ()

let _ =
  packedstring_of_string bitmap_data |> print_packedstring width
