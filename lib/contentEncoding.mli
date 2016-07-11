type mechanism = Rfc2045.mechanism
type field     = [ `ContentEncoding of mechanism ]

val pp            : Format.formatter -> mechanism -> unit

val default       : mechanism

module Encoder :
sig
  val w_encoding  : (mechanism, 'r Encoder.partial) Encoder.k1
  val w_field     : (field, 'r Encoder.partial) Encoder.k1
end

val of_string     : ?chunk:int -> string -> mechanism option
val of_string_raw : ?chunk:int -> string -> int -> int -> (mechanism * int) option
