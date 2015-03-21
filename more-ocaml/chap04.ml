open Printf

type input = {
  pos_in : unit -> int;
  seek_in : int -> unit;
  input_char : unit -> char;
  in_channel_length : int
}

let input_of_channel ch =
  { pos_in = (fun () -> pos_in ch);
    seek_in = seek_in ch;
    input_char = (fun () -> input_char ch);
    in_channel_length = in_channel_length ch
  }

let input_of_string s =
  let pos = ref 0 in
  { pos_in = (fun () -> !pos);
    seek_in = (fun p -> if p < 0 then invalid_arg "seek" else pos := p);
    input_char = (fun () ->
                  if not (!pos < String.length s) then raise End_of_file;
                  let c = s.[!pos] in pos := !pos + 1; c);
    in_channel_length = String.length s
  }

let rewind inp = inp.seek_in (inp.pos_in () - 1)

let is_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false

let rec skip_separators inp =
  let c = inp.input_char () in
  if not (is_alpha c) then skip_separators inp else rewind inp

let rec collect_chars b inp =
  try 
    let c = inp.input_char () in
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
  read_words (input_of_string s)

type output = {
  output_char : char -> unit;
  out_channel_length : unit -> int
}

let output_of_channel ch =
  { output_char = (fun c -> output_byte ch (int_of_char c));
    out_channel_length = (fun () -> out_channel_length ch)
  }

let output_string out s =
  String.iter out.output_char s

let output_int out n =
  output_string out (string_of_int n)

let output_int_list out l =
  out.output_char '[';
  List.iter (fun n -> output_int out n; output_string out "; ") l;
  output_string out "]\n"

let _ =
  let out = output_of_channel stdout in
  output_int_list out [1;2;3;4;5]

(* Q1 *)
let input_of_char_array cs =
  let pos = ref 0 in
  { pos_in = (fun () -> !pos);
    seek_in = (fun p -> if p < 0 then invalid_arg "seek_in" else pos := p);
    input_char = (fun () -> if !pos < Array.length cs 
                            then let c = cs.(!pos) in pos := !pos + 1; c
                            else raise End_of_file);
    in_channel_length = Array.length cs
  }

(* Q2 *)
let input_string inp n = 
  let b = Buffer.create 20 in
  let rec input_string_aux = function
    | 0 -> Buffer.contents b
    | n -> 
       try
         Buffer.add_char b (inp.input_char ()); 
         input_string_aux (n-1)
       with
         End_of_file -> Buffer.contents b
  in
  input_string_aux n

(* Q6 *)
let output_of_buffer b =
  { output_char = (fun c -> Buffer.add_char b c);
    out_channel_length = (fun () -> Buffer.length b)
  }

let _ =
  let b = Buffer.create 20 in
  let out = output_of_buffer b in
  output_string out "Hello world!";
  printf "%s\n" (Buffer.contents b)
