type field_message = Top.field_message
type field_part    = Top.field_part

type ('a, 'b) message = ('a, 'b) Top.message =
  | Discrete  of MrMime_content.t * field_message list * 'a
  | Extension of MrMime_content.t * field_message list * 'b
  | Composite of MrMime_content.t * field_message list * (MrMime_content.t * field_part list * ('a, 'b) part option) list
and ('a, 'b) part = ('a, 'b) Top.part =
  | PDiscrete  of 'a
  | PExtension of 'b
  | PComposite of (MrMime_content.t * field_part list * ('a, 'b) part option) list

type encoding = ..
type encoding += Base64 of MrMime_base64.result
type encoding += QuotedPrintable of string
type encoding += Raw of string

type content = ..
type content += Unit

module Decoder :
sig
  val p_message   : (MrMime_header.header * (encoding, content) message) MrMime_parser.t
end

module Extension :
sig
  val add_encoding : string -> (unit MrMime_parser.t -> unit MrMime_parser.t -> encoding MrMime_parser.t) -> unit
  val add_content  : string -> (string option -> MrMime_content.t -> field_message list -> content MrMime_parser.t) -> unit
end

val of_string_raw : ?chunk:int -> string -> int -> int -> ((MrMime_header.header * (encoding, content) message) * int) option
