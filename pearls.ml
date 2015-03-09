let rec sni_aux n chks = function
  | [] -> ()
  | x::xs ->
    if x < n then chks.(x) <- true;
    sni_aux n chks xs

let smallest_not_in xs =
  let n = List.length xs in
  let chks = Array.make n false in
  sni_aux n chks xs;
  let r = ref (-1) in
  for i = 0 to (n-1) do
    if !r = -1 && chks.(i) = false then r:= i;
  done;
  !r
