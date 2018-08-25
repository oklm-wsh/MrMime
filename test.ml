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

(* An example, step by step to extract an image from an email. *)

(* 1KiB buffer from the input file *)
let tmp = Bytes.create 1024

(* [read_input] attempts to refill [tmp] from the file email, returning the number of bytes read.
   [close] should be called at the end to release the file handle. *)
let (read_input, close) =
  let ch = open_in_bin "email" in
  let len = Bytes.length tmp in
  ((fun () -> input ch tmp 0 len), (fun () -> close_in ch))

(* MrMime's error handling is still WIP, so for now we wrap its errors in an exception *)
exception MrMimeError of Parser.err

let rec get decoder = match Convenience.decode decoder with
  | `Continue ->
    let n = read_input () in
    Convenience.src decoder tmp 0 n;
    get decoder
  | `Done v -> v
  | `Error exn -> raise (MrMimeError exn)

let get_value v err decoder =
  if get decoder <> v then
    failwith err

(* Avoid having the GIF file printed on the console if you run this interactively *)
let print_string =
  if Unix.isatty Unix.stdout then
    ignore
  else
    print_string

let rec get_b64 decoder_b64 =
  match Base64.decode decoder_b64 with
  | `Continue ->
    let n = read_input () in
    Base64.src decoder_b64 tmp 0 n;
    get_b64 decoder_b64
  | `String s ->
    print_string s;
    get_b64 decoder_b64
  | `End s ->
    print_string s
  | `Dirty s ->
    get_b64 decoder_b64
  | `Error exn -> raise (MrMimeError exn)

let decoder = Convenience.decoder (Input.create_bytes 4096) Message.Decoder.p_header

let (header, content, _) = get decoder
let decoder = Convenience.decoding decoder (Message.Decoder.p_first_part content)
let (content_txt, _) = get decoder
let decoder = Convenience.decoding decoder (Message.Decoder.p_discard_part content)
let () = get_value `Next "Expected next MIME part" decoder
let decoder = Convenience.decoding decoder (Message.Decoder.p_next_part content)
let (content_img, _) = get decoder
let bound       = Message.Decoder.p_bound_of_content content
let decoder_b64 = Base64.decoder bound (Convenience.decoder_src decoder)
let () = get_b64 decoder_b64
let decoder = Convenience.decoding decoder (Message.Decoder.p_end_of_part content)
let () = get_value `End "Expected end of MIME stream" decoder; close ()
