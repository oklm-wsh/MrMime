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

let () = Printexc.record_backtrace true

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
      let n = read_into ~newline input t 0 chunk in
      Input.write_string buffer (Bytes.unsafe_to_string t) 0 n;
      aux (consumed + n)
      @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  let v = aux 0 @@ Parser.run i Message.Decoder.p_message in
  close_in input; v

open Convenience

(* open the email *)
let ch      = open_in "sandbox/168";;
(* create a temporary buffer *)
let tmp     = Bytes.create 1024;;
(* create the input *)
let input   = Input.create_bytes 4096;;
(* create a Header decoder with the input *)
let decoder = decoder input Message.Decoder.p_header;;

(* the first shot need some data *)
let `Continue
  : _ decoding
  = decode decoder;;
(* so, we read the file *)
let n         = read_into ~newline:LF ch tmp 0 1024;;
(* and save the temporary buffer in the decoder *)
let ()        = src decoder tmp 0 n;;

(* we have the header of the email and the content information,
   with [content], we can know the type of the email (and it's a multipart) *)
let (`Done (header, content, _))
  : _ decoding
  = decode decoder;;

(* we can create the next decoder for the next part of the email *)
let decoder = decoding decoder (Message.Decoder.p_first_part content);;

(* we catch the content-type of the first part *)
let (`Done (content_1, _))
  : _ decoding
  = decode decoder;;

(* we store the body of the first part. *)
let decoder = decoding decoder (Message.Decoder.p_store_part content content_1);;

(* as [content_1] describe, the body is a raw-data. *)
let (`Done (`Next (Some (Message.Raw body))
            : [ `Next of Message.encoding option
              | `End of Message.encoding option ]))
  : _ decoding
  = decode decoder;;

(* previously, we receive the [`Next] value, so we have a second part. *)
let decoder = decoding decoder (Message.Decoder.p_next_part content);;

(* we catch the content-type of the second part. *)
let (`Done (content_2))
  : _ decoding
  = decode decoder;;

(* we create the boundary of the second part. *)
let boundary = Message.Decoder.p_bound_of_content content;;

(* we create a specific decoder, [content_2] describe the body as a
   quoted-printable data, we will use the decoder of quoted-printable. *)
let decoder' = QuotedPrintable.Convenience.decoder
  boundary (decoder_src decoder);;

let buffer = Buffer.create 16;;

(* we create a specific decoder for a post-script (the type of the second part
   is a post-script file, as [content_2] describes). So, we can do a compute
   with the quoted-printable. *)
let rec ps decoder' = match QuotedPrintable.Convenience.decode decoder' with
  | `Continue ->
    let n = read_into ~newline:LF ch tmp 0 1024 in
    QuotedPrintable.Convenience.src decoder' tmp 0 n;
    ps decoder'
  | `String raw ->
    (* we can imagine to pass the string to a post-script decoder. *)
    Buffer.add_string buffer raw;
    ps decoder'
  | `End raw ->
    (* this is the end of the part. *)
    Buffer.add_string buffer raw
  | `Error exn -> failwith "ps"
  | `Dirty chr ->
    (* it's a resilient error. *)
    Buffer.add_char buffer chr;
    ps decoder'
;;

(* we execute the user-defined decoder. *)
ps decoder';;

(* and we check if we have an other part. *)
let decoder = decoding decoder (Message.Decoder.p_end_of_part content);;

(* we receive, [`End], so we have no part after! *)
let (`Done (`End
            : [ `End | `Next ]))
  : _ decoding
  = decode decoder;;
