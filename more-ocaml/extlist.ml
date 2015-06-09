(* Extended list functions, and tail-recursive versions of standard
list functions. *)

module List = struct
  include List

  let map f l =
    List.rev_map f l |> List.rev

  let append l1 l2 =
    List.rev_append (List.rev l1) l2

  let (@) = append

  let from lo hi =
    if hi < lo then invalid_arg "from";
    let ns = ref [] in
    for i = lo to hi do ns := i :: !ns done;
    List.rev !ns

  let take n l =
    let rec take_aux n l acc =
      if n <= 0 then acc else
        match l with
        | [] -> acc
        | x::xs -> take_aux (n-1) xs (x::acc)
    in
    List.rev (take_aux n l [])

  let rec drop n l =
    if n <= 0 then l else
      match l with
      | [] -> []
      | _::xs -> drop (n-1) xs

end
