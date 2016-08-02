module Base64          : module type of MrMime_base64
module QuotedPrintable : module type of MrMime_quotedPrintable
module Date            : module type of MrMime_date
module Address         : module type of MrMime_address
module Resent          : module type of MrMime_resent
module Trace           : module type of MrMime_trace
module Header          : module type of MrMime_header
module Content         : module type of MrMime_content
module ContentType     : module type of MrMime_contentType
module ContentEncoding : module type of MrMime_contentEncoding
module MimeVersion     : module type of MrMime_mimeVersion
module MsgID           : module type of MrMime_msgID
module Message         : module type of MrMime_message
module Maildir         : module type of MrMime_maildir

module Parser          : module type of MrMime_parser
module Input           : module type of MrMime_input

module Convenience :
sig
  type 'a decoding =
    [ `Continue
    | `Error of Parser.err
    | `Done of 'a ]

  type ('input, 'a) decoder

  val decoder_src      : ('input, 'a) decoder -> 'input Input.t
  val decoder          : 'input Input.t -> 'a Parser.t -> ('input, 'a) decoder
  val decode           : ('input, 'a) decoder -> 'a decoding
  val src              : ('input, 'a) decoder -> string -> int -> int -> unit
  val decoding         : ('input, 'a) decoder -> 'b Parser.t -> ('input, 'b) decoder
end
