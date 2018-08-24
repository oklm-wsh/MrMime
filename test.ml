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

open Convenience

(** An example, step by step to extract an image from an email. *)

let ch      = open_in "email";;
let tmp     = Bytes.create 1024;;
let buffer  = Input.create_bytes 4096;;

let decoder = decoder buffer Message.Decoder.p_header;;

let rec to_result decoder = match decode decoder with
  | `Continue ->
    let n = input ch tmp 0 1024 in
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
    let n = input ch tmp 0 1024 in
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
