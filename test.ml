#require "lwt"
#require "mrmime"

open Lwt.Infix

let to_crlf input =
  let tmp      = Bytes.create (Lwt_io.buffer_size input) in
  let real_off = ref 0 in
  let off      = ref 0 in
  let len      = Lwt_io.buffer_size input in
  fun buf off' len' ->
    let consume rest next =
      let i = ref 0 in
      let j = ref 0 in

      while !i < rest + next && !j < len' && !off + !i < len
      do
        if Bytes.get tmp (!off + !i) = '\n' && !j + 1 < len'
        then begin
          Bytes.blit "\r\n" 0 buf (off' + !j) 2;
          j := !j + 2;
        end else begin
          Bytes.set buf (off' + !j) (Bytes.get tmp (!off + !i));
          incr j;
        end;

        incr i;
      done;

      off      := !off + !i;
      real_off := !real_off + next;

      Lwt.return !j
    in

    match !real_off = len, !off = !real_off with
    | true, false  ->
      consume (!real_off - !off) 0 >>= fun n ->
      Lwt.return n
    | false, false ->
      let a = (!real_off - !off) in
      Lwt_io.read_into input tmp !real_off (len - !real_off) >>= fun n ->
      consume a n
    | false, true  ->
      real_off := 0;
      off      := 0;
      Lwt_io.read_into input tmp !real_off (len - !real_off) >>= fun n ->
      consume 0 n
    | true, true ->
      real_off := 0;
      off      := 0;
      Lwt_io.read_into input tmp !real_off (len - !real_off) >>= fun n ->
      consume 0 n

let read_into channel buf off len =
  if len + off > Bytes.length buf
     || off < 0
     || len < 0
  then raise (Invalid_argument "index out of bound");

  let last = len + off in

  let rec read_char has_cr remaining =
    assert (remaining >= 0);

    Printf.printf "read> remaining %d; len %d\n%!" remaining len;

    if remaining = 0 then Lwt.return len
    else Lwt.catch
         (fun () ->
          Printf.printf "read> try to read character\n%!";

          Lwt_io.read_char channel >>= function
          | '\n' when not has_cr && remaining >= 2 ->
            Bytes.unsafe_blit "\r\n" 0 buf (last - remaining) 2;
            read_char false (remaining - 2)
          | '\n' when not has_cr && remaining = 1 ->
            let pos = Lwt_io.position channel in
            Lwt_io.set_position channel (Int64.pred pos) >>= fun () ->
            Lwt.return (len - remaining)
          | '\r' ->
            Bytes.set buf (last - remaining) '\r'; read_char true  (pred remaining)
          | chr  ->
            Bytes.set buf (last - remaining) chr;  read_char false (pred remaining))
         (fun exn ->
          Printf.printf "read> nothing to do\n%!";
          Lwt.return (len - remaining))
  in

  read_char false len >>= fun n ->
  Printf.printf "read> total %d\n%!" n; Lwt.return n

let rec of_flow (ch, decoder) =
  let rec aux = function
    | `Read (buff, off, len, k) ->
      read_into ch buff off len >>= fun n ->
      Printf.printf "read> [%d]\n%!" n;
      aux (k n)
    | `Error (err, buff, off, len) ->
      Lwt.fail (Error.Error (`Error (err, buff, off, len)))
    | `Ok message -> Lwt.return message
  in

  aux

let of_filename filename =
  Lwt_io.open_file ~mode:Lwt_io.Input filename

let message input =
  let state = Lexer.make () in
  of_flow (input, state)
    (Grammar.p_message
    (fun header message _ -> `Ok (header, message)) state)

let convert input =
  let buffer = Bytes.create 1024 in

  let rec aux off len =
    read_into input buffer off len >>= function
    | 0 -> Lwt.return ()
    | n -> Printf.printf "%s%!" (Bytes.sub buffer off n);
           if off + n = 1024
           then aux 0 1024
           else aux (off + n) (len - n)
  in

  aux 0 1024
