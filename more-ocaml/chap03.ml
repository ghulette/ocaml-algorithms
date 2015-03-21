open Printf

type 'a point = {
  x : float;
  y : float;
  label : string;
  mutable content : 'a
}

let make_point x y label content = 
  { x; y; label; content }

let string_of_point p =
  p.label ^ " = (" ^ string_of_float p.x ^ ", " ^ string_of_float p.y ^ ")"

let relabel p label = 
  {p with label}

let mirror p = 
  {p with x = p.y; y = p.x}

(* Q1 *)
let _ =
  let x = ref 0 in
  printf "x = %d\n" !x;
  x.contents <- 1;
  printf "x = %d\n" !x

(* Q2 *)
let day_string = function
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> invalid_arg "day_string"

let month_string = function
  | 0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> invalid_arg "month_string"

let print_time () =
  let open Unix in
  let tm = localtime (time ()) in
  let min = tm.tm_min in
  let hr = tm.tm_hour in
  let wday = day_string tm.tm_wday in
  let mday = tm.tm_mday in
  let mon = month_string tm.tm_mon in
  let year = tm.tm_year + 1900 in
  printf "It is %d:%02d on %s %d %s %d\n" hr min wday mday mon year

(* Q4 *)
type ('a,'b,'c) t = {
  a : 'a; b : 'a;
  c : 'b; d : 'b;
  e : 'c; f : 'c
}
