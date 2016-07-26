type ty      =
  [ `Application
  | `Audio
  | `Ietf_token of string
  | `Image
  | `Message
  | `Multipart
  | `Text
  | `Video
  | `X_token of string ]
type subty   =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token of string ]
type value   = [ `String of string | `Token of string ]
type field   = [ `ContentType of Rfc2045.content ]

type content = Rfc2045.content =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list}

val pp_ty         : Format.formatter -> ty -> unit
val pp_subty      : Format.formatter -> subty -> unit
val pp_value      : Format.formatter -> value -> unit
val pp_parameter  : Format.formatter -> (string * value) -> unit
val pp            : Format.formatter -> content -> unit

val default       : content

module Encoder :
sig
  val w_type      : (ty,             'r Encoder.partial) Wrap.k1
  val w_subtype   : (subty,          'r Encoder.partial) Wrap.k1
  val w_value     : (value,          'r Encoder.partial) Wrap.k1
  val w_parameter : (string * value, 'r Encoder.partial) Wrap.k1
  val w_content   : (content,        'r Encoder.partial) Wrap.k1
  val w_field     : (field,          'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  val p_content   : content MrMime_parser.t
end

val of_string     : ?chunk:int -> string -> content option
val of_string_raw : ?chunk:int -> string -> int -> int -> (content * int) option
