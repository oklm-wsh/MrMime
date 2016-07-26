type mechanism =
  [ `Base64
  | `Binary
  | `Bit7
  | `Bit8
  | `Ietf_token of string
  | `QuotedPrintable
  | `X_token of string ]
type field     = [ `ContentEncoding of mechanism ]

val pp            : Format.formatter -> mechanism -> unit

val default       : mechanism

module Encoder :
sig
  val w_encoding  : (mechanism, 'r Encoder.partial) Encoder.k1
  val w_field     : (field, 'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  val p_encoding  : mechanism MrMime_parser.t
end

val of_string     : ?chunk:int -> string -> mechanism option
val of_string_raw : ?chunk:int -> string -> int -> int -> (mechanism * int) option
