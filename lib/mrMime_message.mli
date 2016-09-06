type field_message = Top.field_message
type field_part    = Top.field_part

type ('a, 'b) message = ('a, 'b) Top.message =
  | Discrete  of MrMime_content.t * field_message list * 'a
  | Extension of MrMime_content.t * field_message list * 'b
  | Multipart of MrMime_content.t * field_message list * (MrMime_content.t * field_part list * ('a, 'b) part option) list
  | Message   of MrMime_content.t * field_message list * MrMime_header.header * ('a, 'b) message
and ('a, 'b) part = ('a, 'b) Top.part =
  | PDiscrete  of 'a
  | PExtension of 'b
  | PMultipart of (MrMime_content.t * field_part list * ('a, 'b) part option) list
  | PMessage   of MrMime_header.header * ('a, 'b) message

type encoding = ..
type encoding += Base64 of MrMime_base64.Decoder.result
type encoding += QuotedPrintable of string
type encoding += Raw of string

type content = ..
type content += Unit

module Decoder :
sig
  val p_message          : (MrMime_header.header * (encoding, content) message) MrMime_parser.t
  val p_header           : (MrMime_header.header * MrMime_content.t * [ MrMime_header.field | MrMime_content.field ] list) MrMime_parser.t
  val p_first_part       : MrMime_content.t -> (MrMime_content.t * [ MrMime_content.field | MrMime_header.field ] list) MrMime_parser.t
  val p_next_part        : MrMime_content.t -> (MrMime_content.t * [ MrMime_content.field | MrMime_header.field ] list) MrMime_parser.t
  val p_bound_of_content : MrMime_content.t -> (unit MrMime_parser.t * unit MrMime_parser.t)
  val p_store_part       : MrMime_content.t -> MrMime_content.t -> [ `End of encoding option | `Next of encoding option ] MrMime_parser.t
  val p_discard_part     : MrMime_content.t -> [ `End | `Next ] MrMime_parser.t
  val p_end_of_part      : MrMime_content.t -> [ `End | `Next ] MrMime_parser.t
end

module Extension :
sig
  val add_encoding : string -> (unit MrMime_parser.t -> unit MrMime_parser.t -> encoding MrMime_parser.t) -> unit
  val add_content  : string -> (string option -> MrMime_content.t -> field_message list -> content MrMime_parser.t) -> unit
end

val of_string_raw : ?chunk:int -> string -> int -> int -> ((MrMime_header.header * (encoding, content) message) * int) option
