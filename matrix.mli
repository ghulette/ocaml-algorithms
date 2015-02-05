type t
val size : t -> int * int
val get : t -> int -> int -> float
val make : int -> int -> t
val init : int -> int -> (int -> int -> float) -> t
val init_random : int -> int -> t
val init_identity : int -> int -> t
val init_diag : t -> t
val row : t -> int -> t
val col : t -> int -> t
val diag : t -> t
val reshape : t -> int -> int -> t
val transpose : t -> t
val add : t -> t -> t
val mult : t -> t -> t
val chain_mult : t list -> t
val neg : t -> t
val dot : t -> t -> float
val inv : t -> t
val map : (float -> float) -> t -> t
val map2 : (float -> float -> float) -> t -> t -> t
val from_list : float list -> t
val to_list : t -> float list
val from_string : string -> t
val to_string : t -> string
