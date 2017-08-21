module Base64          = MrMime_base64
module QuotedPrintable = MrMime_quotedPrintable
module Date            = MrMime_date
module Address         = MrMime_address
module Resent          = MrMime_resent
module Trace           = MrMime_trace
module Header          = MrMime_header
module Content         = MrMime_content
module ContentType     = MrMime_contentType
module ContentEncoding = MrMime_contentEncoding
module MimeVersion     = MrMime_mimeVersion
module MsgID           = MrMime_msgID
module Message         = MrMime_message
module Maildir         = MrMime_maildir

module Parser          = MrMime_parser
module Input           = MrMime_input

module Convenience =
struct
  type 'a decoding = [ `Continue | `Error of Parser.err | `Done of 'a ]
  type ('input, 'r) decoder =
    { src           : 'input Input.t
    ; mutable i     : Bytes.t
    ; mutable i_off : int
    ; mutable i_len : int
    ; mutable k     : ('input, 'r) decoder -> 'r decoding }

  let rec loop t = function
    | Parser.Read { k; _ } ->
      let f t =
        Input.write_string
          t.src
          (Bytes.unsafe_to_string t.i)
          t.i_off t.i_len;

        let s = if t.i_len = 0
                then Parser.Complete
                else Parser.Incomplete
        in

        loop t @@ k t.i_len s
      in

      t.k <- f; `Continue
    | Parser.Fail (_marks, exn) -> `Error exn
    | Parser.Done v ->
      let f t = loop t @@ Parser.run t.src (Parser.return v) in
      t.k <- f; `Done v

  let decoder_src t = t.src

  let decoder src decoding =
    { src
    ; i     = Bytes.empty
    ; i_off = 0
    ; i_len = 0
    ;  k    = fun t -> loop t @@ Parser.run t.src decoding }

  let decode t = t.k t

  let decoding t decoding =
    { t with k = fun t -> loop t @@ Parser.run t.src decoding }

  let src t buf off len =
    if (off < 0 || len < 0 || off + len > Bytes.length buf)
    then raise (Invalid_argument "MrMime.src");

    t.i <- buf;
    t.i_off <- off;
    t.i_len <- len;
end
