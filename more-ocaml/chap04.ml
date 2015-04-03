open Printf
open Io

let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false

let rec skip_separators inp =
  let open Input in
  let c = read_char inp in
  if not (is_alpha c) 
  then skip_separators inp 
  else rewind inp

let rec collect_chars b inp =
  let open Input in
  try 
    let c = read_char inp in
    if is_alpha c
    then (Buffer.add_char b c; collect_chars b inp)
    else Buffer.contents b
  with 
    End_of_file -> Buffer.contents b

let read_word inp =
  try
    skip_separators inp;
    Some (collect_chars (Buffer.create 20) inp)
  with
    End_of_file -> None

let read_words inp =
  let rec read_words_aux words =
    match read_word inp with
    | None -> List.rev (List.map String.lowercase words)
    | Some word -> read_words_aux (word :: words)
  in
  read_words_aux []

let _ =
  let s = "There were four of them; more than before." in
  let words = read_words (Input.of_string s) in
  List.iter print_endline words

let write_int_list out l =
  let open Output in
  write_char out '[';
  List.iter (fun n -> write_int out n; write_string out "; ") l;
  write_char out ']'

let _ =
  let out = Output.of_channel stdout in
  write_int_list out [1;2;3;4;5];
  Output.write_char out '\n'

(* Q1 *)
let input_of_char_array cs =
  let open Input in
  let pos = ref 0 in
  { pos_in = (fun () -> !pos);
    seek_in = (fun p -> if p < 0 then invalid_arg "seek_in" else pos := p);
    input_char = (fun () -> if !pos < Array.length cs 
                            then let c = cs.(!pos) in pos := !pos + 1; c
                            else raise End_of_file);
    in_channel_length = Array.length cs
  }

(* Q2 *)
let read_string inp n =
  let open Input in
  let b = Buffer.create 20 in
  let rec input_string_aux = function
    | 0 -> Buffer.contents b
    | n -> 
       try
         let c = read_char inp in
         Buffer.add_char b c;
         input_string_aux (n-1)
       with
         End_of_file -> Buffer.contents b
  in
  input_string_aux n

(* Q6 *)
let _ =
  let b = Buffer.create 20 in
  let out = Output.of_buffer b in
  Output.write_string out "Hello world!";
  printf "%s\n" (Buffer.contents b)
