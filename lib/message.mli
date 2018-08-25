type field_message = Top.field_message
type field_part    = Top.field_part

type ('a, 'b) message = ('a, 'b) Top.message =
  | Discrete  of Content.t * field_message list * 'a
  | Extension of Content.t * field_message list * 'b
  | Multipart of Content.t * field_message list * (Content.t * field_part list * ('a, 'b) part option) list
  | Message   of Content.t * field_message list * Header.header * ('a, 'b) message
and ('a, 'b) part = ('a, 'b) Top.part =
  | PDiscrete  of 'a
  | PExtension of 'b
  | PMultipart of (Content.t * field_part list * ('a, 'b) part option) list
  | PMessage   of Header.header * ('a, 'b) message

type encoding = ..
type encoding += Base64 of Base64.Decoder.result
type encoding += QuotedPrintable of string
type encoding += Raw of string

type content = ..
type content += Unit

module Decoder :
sig
  val p_message          : (Header.header * (encoding, content) message) Parser.t
  val p_header           : (Header.header * Content.t * [ Header.field | Content.field ] list) Parser.t
  val p_first_part       : Content.t -> (Content.t * [ Content.field | Header.field ] list) Parser.t
  val p_next_part        : Content.t -> (Content.t * [ Content.field | Header.field ] list) Parser.t
  val p_bound_of_content : Content.t -> (unit Parser.t * unit Parser.t)
  val p_store_part       : Content.t -> Content.t -> [ `End of encoding option | `Next of encoding option ] Parser.t
  val p_discard_part     : Content.t -> [ `End | `Next ] Parser.t
  val p_end_of_part      : Content.t -> [ `End | `Next ] Parser.t
end

module Extension :
sig
  val add_encoding : string -> (unit Parser.t -> unit Parser.t -> encoding Parser.t) -> unit
  val add_content  : string -> (string option -> Content.t -> field_message list -> content Parser.t) -> unit
end

val of_string_raw : ?chunk:int -> string -> int -> int -> ((Header.header * (encoding, content) message) * int) option
