open Chap04

let int_of_bool b = if b then 1 else 0

type input_bits = {
  input : input;
  mutable byte : int;
  mutable bit : int
}

let input_bits_of_input input = 
  { input; byte = 0; bit = 0 }

let rec get_bit bits =
  if bits.bit = 0 then
    begin
      bits.byte <- int_of_char (bits.input.input_char ());
      bits.bit <- 1 lsl 7;
      get_bit bits
    end
  else
    let r = bits.byte land bits.bit > 0 in
    bits.bit <- bits.bit lsr 1; r

let align bits =
  bits.bit <- 0

let get_int_val bits n =
  if n <= 0 || n > Sys.word_size-1 then invalid_arg "get_int_val";
  let r = ref 0 in
  for i = n-1 downto 0 do
    let x = if get_bit bits then 1 else 0 in
    r := !r lor (x lsl i)
  done; !r

type output_bits = {
  output : output;
  mutable obyte : int;
  mutable obit : int
}

let output_bits_of_output output =
  { output; obyte = 0; obit = 7 }

let flush o =
  if o.obit < 7 then o.output.output_char (char_of_int o.obyte);
  o.obyte <- 0;
  o.obit <- 7

let rec put_bit o b =
  if o.obit = (-1) then
    begin
      flush o; 
      put_bit o b
    end
  else
    begin
      if b <> 0 then o.obyte <- o.obyte lor (1 lsl o.obit);
      o.obit <- o.obit - 1
    end

let put_int_val o v l =
  for i = l - 1 downto 0 do
    put_bit o (v land (1 lsl i))
  done

let put_bool_val o b =
  put_bit o (int_of_bool b)

module type BitVector =
  sig
    type t
    val size : int
    val zero : t
    val one : t
    val ( lor ) : t -> t -> t
    val ( land ) : t -> t -> t
    val ( lsl ) : t -> int -> t
    val ( lsr ) : t -> int -> t
  end

module Int32_BV : BitVector with type t = Int32.t =
  struct
    type t = Int32.t
    let size = 32
    let zero = Int32.zero
    let one = Int32.one
    let ( lor ) = Int32.logor
    let ( land ) = Int32.logand
    let ( lsl ) = Int32.shift_left
    let ( lsr ) = Int32.shift_right
  end

module Int64_BV : BitVector with type t = Int64.t =
  struct
    type t = Int64.t
    let size = 64
    let zero = Int64.zero
    let one = Int64.one
    let ( lor ) = Int64.logor
    let ( land ) = Int64.logand
    let ( lsl ) = Int64.shift_left
    let ( lsr ) = Int64.shift_right
  end

module BitValue (V : BitVector) = struct
  let get_val bits n =
    if n <= 0 || n > V.size then invalid_arg "get_val";
    let r = ref V.zero in
    for i = n-1 downto 0 do
      let x = if get_bit bits then V.one else V.zero in
      r := V.(!r lor (x lsl i))
    done;
    !r

  let put_val o v l =
    for i = l - 1 downto 0 do
      let b = V.(v land (one lsl i)) in
      put_bool_val o (b <> V.zero)
    done
end

module BitValue_Int32 = BitValue (Int32_BV)
let get_int32_val = BitValue_Int32.get_val
let put_int32_val = BitValue_Int32.put_val


type tcp_header = {
  src_port : int;
  dst_port : int;
  sequence : Int32.t;
  ack : Int32.t;
  offset : int;
  reserved : int;
  flag_urgent : bool;
  flag_ack : bool;
  flag_push : bool;
  flag_reset : bool;
  flag_syn : bool;
  flag_fin : bool;
  recv_win_size : int;
  checksum : int;
  urgent_ptr : int
}

let decode s =
  let inp = input_of_string s in
  let b = input_bits_of_input inp in
  let src_port = get_int_val b 16 in
  let dst_port = get_int_val b 16 in
  let sequence = get_int32_val b 32 in
  let ack = get_int32_val b 32 in
  let offset = get_int_val b 4 in
  let reserved = get_int_val b 6 in
  let flag_urgent = get_bit b in
  let flag_ack = get_bit b in
  let flag_push = get_bit b in
  let flag_reset = get_bit b in
  let flag_syn = get_bit b in
  let flag_fin = get_bit b in
  let recv_win_size = get_int_val b 16 in
  let checksum = get_int_val b 16 in
  let urgent_ptr = get_int_val b 16 in
  { src_port; dst_port; sequence; ack; offset; reserved;
    flag_urgent; flag_ack; flag_push; flag_reset; flag_syn; flag_fin;
    recv_win_size; checksum; urgent_ptr }

let encode h =
  let buf = Buffer.create 20 in
  let out = output_of_buffer buf in
  let b = output_bits_of_output out in
  put_int_val b h.src_port 16;
  put_int_val b h.dst_port 16;
  put_int32_val b h.sequence 32;
  put_int32_val b h.ack 32;
  put_int_val b h.offset 4;
  put_int_val b h.reserved 6;
  put_bool_val b h.flag_urgent;
  put_bool_val b h.flag_ack;
  put_bool_val b h.flag_push;
  put_bool_val b h.flag_reset;
  put_bool_val b h.flag_syn;
  put_bool_val b h.flag_fin;
  put_int_val b h.recv_win_size 16;
  put_int_val b h.checksum 16;
  put_int_val b h.urgent_ptr 16;
  flush b;
  Buffer.contents buf

let print_header h =
  let open Printf in
  printf "Source port = %d\n" h.src_port;
  printf "Destination port = %d\n" h.dst_port;
  printf "Sequence = %sl\n" (Int32.to_string h.sequence);
  printf "Acknowledgement = %sl\n" (Int32.to_string h.ack);
  printf "Urgent = %B\n" h.flag_urgent;
  printf "Ack = %B\n" h.flag_ack;
  printf "Push = %B\n" h.flag_push;
  printf "Reset = %B\n" h.flag_reset;
  printf "Syn = %B\n" h.flag_syn;
  printf "Fin = %B\n" h.flag_fin;
  printf "Receive window size = %d\n" h.recv_win_size;
  printf "Checksum = %d\n" h.checksum;
  printf "Urgent ptr = %d\n" h.urgent_ptr

let print_hex_string s =
  let open Printf in
  printf "\"";
  for i = 0 to String.length s - 1 do
    printf "\\x%02x" (int_of_char s.[i])
  done;
  printf "\"\n"

let tcp_datagram = "\x00\x26\xbb\x14\x62\xb7\xcc\x33\x58\x55\
                    \x1e\xed\x08\x00\x45\x00\x03\x78\xf7\xac"

let _ =
  let open Printf in

  printf "== Input ==\n";
  print_hex_string tcp_datagram;

  printf "== Decoded ==\n";
  let h = decode tcp_datagram in
  print_header h;

  printf "== Re-encoded ==\n";
  let d = encode h in
  print_hex_string d;

  printf "== Re-decoded ==\n";
  let h' = decode d in
  print_header h';
  
  if d <> tcp_datagram then failwith "Wrong!"

