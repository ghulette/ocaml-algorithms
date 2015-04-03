module Input = struct

  type t = { 
    pos_in : unit -> int;
    seek_in : int -> unit;
    input_char : unit -> char;
    in_channel_length : int
  }

  let of_channel ch =
    { pos_in = (fun () -> pos_in ch);
      seek_in = seek_in ch;
      input_char = (fun () -> input_char ch);
      in_channel_length = in_channel_length ch
    }

  let of_string s =
    let pos = ref 0 in
    { pos_in = (fun () -> !pos);
      seek_in = (fun p -> if p < 0 then invalid_arg "seek" else pos := p);
      input_char = (fun () ->
                    if not (!pos < String.length s) then raise End_of_file;
                    let c = s.[!pos] in pos := !pos + 1; c);
      in_channel_length = String.length s
    }

  let length inp =
    inp.in_channel_length

  let read_char inp =
    inp.input_char ()

  let rewind inp = 
    inp.seek_in (inp.pos_in () - 1)

end


module Output = struct

  type t = {
    output_char : char -> unit;
    out_channel_length : unit -> int
  }

  let of_channel ch =
    { output_char = (fun c -> int_of_char c |> output_byte ch);
      out_channel_length = (fun () -> out_channel_length ch)
    }

  let of_buffer b =
    { output_char = Buffer.add_char b;
      out_channel_length = (fun () -> Buffer.length b)
    }

  let length out =
    out.out_channel_length ()

  let write_char out c =
    out.output_char c

  let write_string out s =
    String.iter (write_char out) s

  let write_int out n =
    write_string out (string_of_int n)

end

module BitInput = struct
  type t = {
    input : Input.t;
    mutable byte : int;
    mutable bit : int
  }

  let of_input inp = 
    { input = inp; byte = 0; bit = 0 }

  let align ib =
    ib.bit <- 0

  let rec read_bit ib =
    if ib.bit = 0 then
      begin
        ib.byte <- Input.read_char ib.input |> int_of_char;
        ib.bit <- 1 lsl 7;
        read_bit ib
      end
    else
      let r = ib.byte land ib.bit > 0 in
      ib.bit <- ib.bit lsr 1;
      r

  let read_int ib n =
    if n <= 0 || n > Sys.word_size-1 then invalid_arg "get_int_val";
    let r = ref 0 in
    for i = n-1 downto 0 do
      let x = if read_bit ib then 1 else 0 in
      r := !r lor (x lsl i)
    done;
    !r

end


module BitOutput = struct

  type t = {
    output : Output.t;
    mutable byte : int;
    mutable bit : int
  }

  let of_output out =
    { output = out; byte = 0; bit = 7 }

  let flush ob =
    if ob.bit < 7 then Output.write_char ob.output (char_of_int ob.byte);
    ob.byte <- 0;
    ob.bit <- 7

  let rec write_bit ob b =
    if ob.bit = (-1) then
      begin
        flush ob;
        write_bit ob b
      end
    else
      begin
        if b <> 0 then ob.byte <- ob.byte lor (1 lsl ob.bit);
        ob.bit <- ob.bit - 1
      end

  let write_int ob v l =
    for i = l - 1 downto 0 do
      write_bit ob (v land (1 lsl i))
    done

  let write_bool ob b =
    write_bit ob (if b then 1 else 0)

end
