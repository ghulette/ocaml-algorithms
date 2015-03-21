let rec fold_left f a = function
  | [] -> a
  | x::xs -> fold_left f (f a x) xs

let rec fold_right f l a =
  match l with
  | [] -> a
  | x::xs -> f x (fold_right f xs a)

let fold_right' f l a =
  List.rev (fold_left f a l)

let all l = 
  fold_left ( && ) true l

let any l =
  fold_left ( || ) false l

let setify l = 
  fold_left (fun m x -> if List.mem x m then m else x::m) [] l

let map f xs = 
  fold_right (fun x a -> f x :: a) xs []

let append xs ys =
  fold_right (fun x a -> x :: a) xs ys

let split xys =
  fold_right (fun (x,y) (xs,ys) -> (x::xs,y::ys)) xys ([],[])

type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree

let rec fold_tree f a = function
  | Lf -> a
  | Br (x,l,r) -> f x (fold_tree f a l) (fold_tree f a r)

let ex1 = Br (1, Br (0, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf))

let tree_size t =
  fold_tree (fun _ l r -> l + r + 1) 0 t

let tree_sum t =
  fold_tree (fun x l r -> l + r + x) 0 t

let tree_preorder t =
  fold_tree (fun x l r -> [x] @ l @ r) [] t

let tree_inorder t =
  fold_tree (fun x l r -> l @ [x] @ r) [] t

let tree_postorder t =
  fold_tree (fun x l r -> l @ r @ [x]) [] t

(* Q1 *)
let balance_budget budget =
  fold_left (fun total expense -> total - expense) budget

(* Q2 *)
let len l =
  fold_left (fun a _ -> a + 1) 0 l

(* Q3 *)
let last l =
  fold_right (fun x _ -> Some x) l None

(* Q4 *)
let rev l =
  fold_left (fun a x -> x :: a) [] l

(* Q5 *)
let mem x l =
  fold_left (fun b y -> b || x == y) false l

(* Q6 *)
let tree_depth t =
  fold_tree (fun _ l r -> 1 + (max l r)) 0 t
