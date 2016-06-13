type t

type field = [ `MimeVersion of t ]

val field_of_lexer : Rfc2045.mime_field -> field

val make    : int -> int -> t
val default : t

module D :
sig
  val of_lexer  : Rfc2045.version -> (t, 'r) Decoder.k1
  val of_lexer' : Rfc2045.version -> t
end

module E :
sig
  val w : (field, 'r Encoder.partial) Encoder.k1
end

val equal : t -> t -> bool
val pp    : Format.formatter -> t -> unit
