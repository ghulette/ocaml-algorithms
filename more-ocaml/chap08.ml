open Printf

(* Q1 *)

let pairs_to_string sep ps =
  let b = Buffer.create 128 in
  let rec pairs_to_string_aux = function
    | [] -> ""
    | [(x, y)] ->
       bprintf b "(%d, %d)" x y;
       Buffer.contents b
    | (x, y)::ps ->
       bprintf b "(%d, %d) %s " x y sep;
       pairs_to_string_aux ps
  in
  pairs_to_string_aux ps

let _ =
  let ps = [(1, 2); (5, 6); (6, 6); (7, 5)] in
  printf "%s\n" (pairs_to_string "-->" ps)


(* Q2 *)

let hex_string s =
  let b = Buffer.create (String.length s * 2) in
  let hex_char c = bprintf b "%x" (Char.code c) in
  String.iter hex_char s;
  Buffer.contents b

let _ =
  printf "%s\n" (hex_string "Hello!")


(* Q3 *)

let _ =
  let mkstring () = "string" in
  printf "%s\n" (mkstring ())


(* Q4 *)

let print_table ?(width = 10) ns =
  List.iter (printf "(%*d)\n" width) ns

let _ =
  let ns = [1; 23; 234125; 0] in
  print_table ns
