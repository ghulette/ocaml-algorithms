module Input : sig
  type t = { 
    pos_in : unit -> int;
    seek_in : int -> unit;
    input_char : unit -> char;
    in_channel_length : int
  }
  val of_channel : in_channel -> t
  val of_string : string -> t
  val length : t -> int
  val read_char : t -> char
  val rewind : t -> unit
end

module Output : sig
  type t = {
    output_char : char -> unit;
    out_channel_length : unit -> int
  }
  val of_channel : out_channel -> t
  val of_buffer : Buffer.t -> t
  val length : t -> int
  val write_char : t -> char -> unit
  val write_string : t -> string -> unit
  val write_int : t -> int -> unit
end

module BitInput : sig
  type t
  val of_input : Input.t -> t
  val align : t -> unit
  val read_bool : t -> bool
  val read_int : t -> int -> int
end

module BitOutput : sig
  type t
  val of_output : Output.t -> t
  val flush : t -> unit
  val write_bool : t -> bool -> unit
  val write_int : t -> int -> int -> unit
end
