module List = struct
  include List

  let take n xs =
    let rec take_aux l n xs =
      if n == 0 then List.rev l else
        match xs with
        | [] -> List.rev l
        | x::xs -> take_aux (x::l) (n-1) xs
    in
    take_aux [] n xs

  let take_while p xs =
    let rec take_while_aux l p = function
      | [] -> List.rev l
      | x::xs -> if p x then take_while_aux (x::l) p xs else List.rev l
    in
    take_while_aux [] p xs

  let rec drop n xs =
    match (n,xs) with
    | (0,_) -> xs
    | (_,[]) -> []
    | (n,_::xs') -> drop (n-1) xs'

  let rec drop_while p = function
    | [] -> []
    | x::xs -> if p x then drop_while p xs else xs
end

let id x = x

let smallest_not_in xs =
  let n = List.length xs in
  let chks = Array.make n false in
  List.iter (fun i -> if i < n then chks.(i) <- true) xs;
  Array.to_list chks |> List.take_while id |> List.length
