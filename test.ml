#require "mrmime";;

#install_printer MrMime.Address.pp
#install_printer MrMime.Content.pp
#install_printer MrMime.ContentType.pp
#install_printer MrMime.ContentEncoding.pp
#install_printer MrMime.Date.pp
#install_printer MrMime.Header.pp
#install_printer MrMime.MsgID.pp
#install_printer MrMime.MimeVersion.pp

open MrMime

type newline =
  | CRLF
  | LF

let read_into ?(newline = LF) channel buf off len =
  if len + off > Bytes.length buf
     || off < 0
     || len < 0
  then raise (Invalid_argument "index out of bound");

  let last = len + off in

  match newline with
  | CRLF -> input channel buf off len
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

let of_filename filename =
  open_in filename

let to_filename filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let message ?(chunk = 1024) ?(newline = LF) input =
  let i = Input.create_bytes chunk in
  let t = Bytes.create chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      Format.printf "%a\n%!" Input.pp buffer;
      let n = read_into ~newline input t 0 chunk in
      Input.write_string buffer (Bytes.unsafe_to_string t) 0 n;
      aux (consumed + n)
      @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  let v = aux 0 @@ Parser.run i Message.Decoder.p_message in
  close_in input; v

open Convenience

(** An example, step by step to extract an image from an email. *)

let ch      = open_in "email";;
let tmp     = Bytes.create 1024;;
let input   = Input.create_bytes 4096;;

let decoder = decoder input Message.Decoder.p_header;;

let rec to_result decoder = match decode decoder with
  | `Continue ->
    let n = read_into ~newline:LF ch tmp 0 1024 in
    src decoder tmp 0 n;
    to_result decoder
  | `Done v -> Ok v
  | `Error exn -> Error exn

let Ok (header, content, _) = to_result decoder
let decoder = decoding decoder (Message.Decoder.p_first_part content)
let Ok (content_txt, _) = to_result decoder
let decoder = decoding decoder (Message.Decoder.p_discard_part content)
let Ok (`Next : [`Next | `End ]) = to_result decoder
let decoder = decoding decoder (Message.Decoder.p_next_part content)
let Ok (content_img, _) = to_result decoder

let bound       = Message.Decoder.p_bound_of_content content
let decoder_b64 = Base64.decoder bound (decoder_src decoder)

let rec b64_to_result decoder_b64 =
  let open Base64 in
  match decode decoder_b64 with
  | `Continue ->
    let n = read_into ~newline:LF ch tmp 0 1024 in
    src decoder_b64 tmp 0 n;
    b64_to_result decoder_b64
  | `String s ->
    print_string s;
    b64_to_result decoder_b64
  | `End s ->
    print_string s;
    Ok ()
  | `Dirty s ->
    b64_to_result decoder_b64
  | `Error exn -> Error exn

let Ok () = b64_to_result decoder_b64

let decoder = decoding decoder (Message.Decoder.p_end_of_part content)
let Ok (`End : [ `Next | `End ]) = to_result decoder
