#require "lwt"
#require "mrmime"
#install_printer Content.pp
#install_printer ContentType.pp
#install_printer ContentEncoding.pp
#install_printer MsgID.pp
#install_printer Date.pp
#install_printer Message.pp
#install_printer Header.pp
#install_printer Trace.pp
#install_printer Resent.pp

open Lwt.Infix

let () = Printexc.record_backtrace true

type newline =
  | CRLF
  | LF
  | CR

let read_into ?(newline = LF) channel buf off len =
  if len + off > Bytes.length buf
     || off < 0
     || len < 0
  then raise (Invalid_argument "index out of bound");

  let last = len + off in

  match newline with
  | CRLF -> Lwt_io.read_into channel buf off len
  | CR ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then Lwt.return len
      else Lwt.catch
             (fun () -> Lwt_io.read_char channel >>= fun chr -> Lwt.return (Some chr))
             (fun exn -> Lwt.return None)
           >>= function
           | Some '\n' when has_cr ->
             read_char false remaining
           | Some '\r' ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char true  (remaining - 2)
           | Some chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | None -> Lwt.return (len - remaining)
    in

    read_char false len
  | LF ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then Lwt.return len
      else Lwt.catch
             (fun () -> Lwt_io.read_char channel >>= fun chr -> Lwt.return (Some chr))
             (fun exn -> Lwt.return None)
           >>= function
           | Some '\n' when not has_cr && remaining >= 2 ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char false (remaining - 2)
           | Some '\n' when not has_cr && remaining = 1 ->
             let pos = Lwt_io.position channel in
             Lwt_io.set_position channel (Int64.pred pos) >>= fun () ->
             Lwt.return (len - remaining)
           | Some '\r' ->
             Bytes.set buf (last - remaining) '\r';
             read_char true  (pred remaining)
           | Some chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | None -> Lwt.return (len - remaining)
    in

    read_char false len



let rec of_flow (ch, decoder) newline =
  let rec aux = function
    | `Read (buff, off, len, k) ->
      read_into ~newline ch buff off len >>= fun n ->
      aux (k n)
    | `Error (err, buff, off, len) ->
      Lwt.fail (Error.Error (`Error (err, buff, off, len)))
    | `Ok message -> Lwt.return message
  in

  aux

let of_filename filename =
  Lwt_io.open_file ~mode:Lwt_io.Input filename

let message ?(newline = LF) input =
  let state = Decoder.make () in
  of_flow (input, state) newline
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

let read_into' ?(newline = LF) channel buf off len =
  if len + off > Bytes.length buf
     || off < 0
     || len < 0
  then raise (Invalid_argument "index out of bound");

  let last = len + off in

  match newline with
  | CRLF -> input channel buf off len
  | CR ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then len
      else match input_char channel with
           | '\n' when has_cr ->
             read_char false remaining
           | '\r' ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char true  (remaining - 2)
           | chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | exception End_of_file -> (len - remaining)
    in

    read_char false len
  | LF ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then len
      else match input_char channel with
           | '\n' when not has_cr && remaining >= 2 ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char false (remaining - 2)
           | '\n' when not has_cr && remaining = 1 ->
             let pos = pos_in channel in
             seek_in channel (pred pos);
             (len - remaining)
           | '\r' ->
             Bytes.set buf (last - remaining) '\r';
             read_char true  (pred remaining)
           | chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | exception End_of_file -> (len - remaining)
    in

    read_char false len

let rec of_flow' (ch, decoder) newline =
  let rec aux = function
    | `Read (buff, off, len, k) ->
      let n = read_into' ~newline ch buff off len in
      aux (k n)
    | `Error (err, buff, off, len) ->
      raise (Error.Error (`Error (err, buff, off, len)))
    | `Ok message -> (message : Message.t)
  in

  aux

let of_filename' filename =
  open_in filename

let to_filename' filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let message' ?(newline = LF) input =
  let state = Decoder.make () in
  of_flow' (input, state) newline
    (Grammar.p_message
    (fun header message _ -> `Ok (header, message)) state)
