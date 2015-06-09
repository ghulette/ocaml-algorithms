let count_char c s i =
  let rec count_char_aux i acc =
    if i < String.length s && s.[i] = c
    then count_char_aux (i+1) (acc+1)
    else acc
  in
  count_char_aux i 0

(* TODO: This only checks the start of the string *)
let search p s =
  let rec search_aux pi si =
    match p.[pi] with
    | '?' ->
       let c = p.[pi+1] in
       let inc = if s.[si] = c then 1 else 0 in
       search_aux (pi+2) (si+inc)
    | '*' ->
       let c = p.[pi+1] in
       let n = count_char c s si in
       search_aux (pi+2) (si+n)
    | '+' ->
       let c = p.[pi+1] in
       let n = count_char c s si in
       n > 0 && search_aux (pi+2) (si+n)
    | '\\' ->
       let c = p.[pi+1] in
       s.[si] = c && search_aux (pi+2) (si+1)
    | c ->
       s.[si] = c && search_aux (pi+1) (si+1)
  in
  search_aux 0 0
