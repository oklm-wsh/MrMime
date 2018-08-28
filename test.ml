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

(* Parsing in MrMime works by passing an input buffer (Input.t) and parser (Parser.t) to Parser.run.
   Parser.run will process the available input using the parser and either yield a parsed value, a
   parsing error or a request for more data with a continuation. More data should be put in the
   input buffer and then the continuation may be called. This pattern is wrapped by the Convenience
   module whose utility is to squirrel away the continuation while more bytes are fed to the input.
   The function get wraps this further by reading a chunks from our input file until parsing yields
   a value. *)
let rec get decoder =
  match Convenience.decode decoder with
  | `Continue ->
    (* Read another chunk, *)
    let n = read_input () in
    (* hand it to the decoder, *)
    Convenience.src decoder tmp 0 n;
    (* and carry on parsing *)
    get decoder
  | `Done v -> v
  | `Error exn -> raise (MrMimeError exn)

(* [get_value] runs the [decoder] and raises [Failure] *)
let get_value v err (decoder : ('input, 'a) Convenience.decoder) =
  if get decoder <> v then
    failwith err

(* Avoid having the GIF file printed on the console if you run this interactively *)
let print_string =
  if Unix.isatty Unix.stdout then
    ignore
  else
    print_string

(* The Base64 module includes functions as in the general Convenience module, but with a more
   detailed return type, to allow for best-effort decoding of corrupted Base64 files. This function
   is therefore similar in structure to [get]. *)
let rec get_b64 decoder_b64 =
  match Base64.decode decoder_b64 with
  | `Continue ->
    (* Read another chunk, *)
    let n = read_input () in
    (* hand it to the Base64 decoder, *)
    Base64.src decoder_b64 tmp 0 n;
    (* and carry on decoding it *)
    get_b64 decoder_b64
  | `String s ->
    (* A chunk of decoded data *)
    print_string s;
    get_b64 decoder_b64
  | `End s ->
    (* Decoding complete *)
    print_string s
  | `Dirty s ->
    (* Ignore this chunk and carry on (shouldn't happen in this demo!) *)
    get_b64 decoder_b64
  | `Error exn -> raise (MrMimeError exn)

(* Now let's process the actual input message. The most general way of doing this is to use a
   Message.Decoder.p_message which returns a structure containing the entire message fully parsed:

   let message = get (Convenience.decoder (Input.create_bytes 4096) Message.Decoder.p_message)

   However, it's also possible to walk the structure of the message, only decoding the parts
   required. For this, we start off with a Message.Decoder.p_header:
*)

let decoder = Convenience.decoder (Input.create_bytes 4096) Message.Decoder.p_header
(* XXX What are the fields? *)
let (header, content, unknown_fields) = get decoder

(* The headers are now parsed, so we must switch to a different decoder which is done using the
   decoding function in Convenience. *)
let decoder = Convenience.decoding decoder (Message.Decoder.p_first_part content)
(* XXX What's the other bit? *)
let (content_msg, unknown_fields) = get decoder

(* The test message is a multipart/related and the parser is now positioned just before the message
   portion. This text could be retrieved with:

   let decoder = Convenience.decoding decoder (Message.Decoder.p_first_part content_msg)
   let (msg_txt, unknown_fields) = get decoder
   (* XXX What can be done here to read the text? *)
   let decoder = Convenience.decoding decoder (Message.Decoder.p_discard_part content_msg)
   let () = get_value `Next "Unexpected end of multipart/alternative" decoder
   let decoder = Convenience.decoding decoder (Message.Decoder.p_next_part content_msg)
   let (msg_html, unknown_fields) = get decoder
   (* XXX Similarly, what reads the text? *)
   let decoder = Convenience.decoding decoder (Message.Decoder.p_discard_part content_msg)
   let () = get_value `End "Expected end of multipart/alternative" decoder

   However, we're trying to extract the GIF attachment, so we just discard this part instead:
*)
let decoder = Convenience.decoding decoder (Message.Decoder.p_discard_part content)

(* p_discard_part yields either `Next (or `End, if there are no more message parts) *)
let () = get_value `Next "Expected next MIME part" decoder

(* Having discarded the message portion,  we want to move to parse the attachment which is done
   using Message.Decoder.p_next_part: *)
let decoder = Convenience.decoding decoder (Message.Decoder.p_next_part content)
(* XXX Again, what's the other bit? *)
let (content_gif, unknown_fields) = get decoder

(* There are two ways of reading the attachment. We could use the very general
   Message.Decoder.p_store_part:

   let decoder = Convenience.decoding decoder (Message.Decoder.p_store_part content content_gif);;
   get decoder

   but as we know from content_gif that we're definitely looking at a Base64 stream, we can also do
   the streaming manually, which is obviously better for large data:
*)

(* First we need the content boundary so that the Base64 decoder knows when to stop. This
   attachment is part of the multipart/related which is the top-level of the message, so we use
   Message.Decoder.p_bound_of_content on [content]: *)
let bound = Message.Decoder.p_bound_of_content content

(* We can now set-up a Base64 decoder... *)
let decoder_b64 = Base64.decoder bound (Convenience.decoder_src decoder)

(* ... and extract the GIF *)
let () = get_b64 decoder_b64

(* The Base64 decoder will have left the parser at the boundary, so we must parse that: *)
let decoder = Convenience.decoding decoder (Message.Decoder.p_end_of_part content)

(* This should be the end of the test message. If there were more attachments, we'd get `Next. *)
let () = get_value `End "Expected end of MIME stream" decoder; close ()
