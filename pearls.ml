open Basics

let smallest_not_in xs =
  let n = List.length xs in
  let chks = Array.make n false in
  List.iter (fun i -> if i < n then chks.(i) <- true) xs;
  Array.to_list chks |> List.take_while Func.id |> List.length
