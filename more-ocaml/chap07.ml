(* Q1 *)
module ArrayLabels = struct
  include ArrayLabels
            
  let make ~length ~elt =
    ArrayLabels.make length elt

end

(* Q2 *)
type start = Start of int
type length = Length of int

let fill a (Start start) (Length length) v =
  for x = start to start + length - 1 do a.(x) <- v done

(* Q4 *)
let rec map ?(a = []) f = function
  | [] -> List.rev a
  | x :: xs -> map ~a:(f x :: a) f xs
