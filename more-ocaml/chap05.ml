open Io

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
  let read ib n =
    if n <= 0 || n > V.size then invalid_arg "read";
    let r = ref V.zero in
    for i = n-1 downto 0 do
      let b = BitInput.read_bool ib in
      let x = if b then V.one else V.zero in
      r := V.(!r lor (x lsl i))
    done;
    !r

  let write ob v l =
    for i = l - 1 downto 0 do
      let b = V.(v land (one lsl i)) in
      BitOutput.write_bool ob (b <> V.zero)
    done
end

module BitValue_Int32 = BitValue (Int32_BV)
let read_int32 = BitValue_Int32.read
let write_int32 = BitValue_Int32.write

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
  let b = BitInput.of_input (Input.of_string s) in
  let open BitInput in
  let src_port = read_int b 16 in
  let dst_port = read_int b 16 in
  let sequence = read_int32 b 32 in
  let ack = read_int32 b 32 in
  let offset = read_int b 4 in
  let reserved = read_int b 6 in
  let flag_urgent = read_bool b in
  let flag_ack = read_bool b in
  let flag_push = read_bool b in
  let flag_reset = read_bool b in
  let flag_syn = read_bool b in
  let flag_fin = read_bool b in
  let recv_win_size = read_int b 16 in
  let checksum = read_int b 16 in
  let urgent_ptr = read_int b 16 in
  { src_port; dst_port; sequence; ack; offset; reserved;
    flag_urgent; flag_ack; flag_push; flag_reset; flag_syn; flag_fin;
    recv_win_size; checksum; urgent_ptr }

let encode h =
  let buf = Buffer.create 20 in
  let b = BitOutput.of_output (Output.of_buffer buf) in
  let open BitOutput in
  write_int b h.src_port 16;
  write_int b h.dst_port 16;
  write_int32 b h.sequence 32;
  write_int32 b h.ack 32;
  write_int b h.offset 4;
  write_int b h.reserved 6;
  write_bool b h.flag_urgent;
  write_bool b h.flag_ack;
  write_bool b h.flag_push;
  write_bool b h.flag_reset;
  write_bool b h.flag_syn;
  write_bool b h.flag_fin;
  write_int b h.recv_win_size 16;
  write_int b h.checksum 16;
  write_int b h.urgent_ptr 16;
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

let tcp_datagram = 
  "\x00\x26\xbb\x14\x62\xb7\xcc\x33\x58\x55\
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

