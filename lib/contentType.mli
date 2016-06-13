type ty    = Rfc2045.ty
type subty = Rfc2045.subty
type value = Rfc2045.value
type t

val ty         : t -> ty
val subty      : t -> subty
val parameters : t -> (string * Rfc2045.value) list

val make : ?parameters:(string * value) list -> ty -> subty -> t
val default : t

type field = [ `ContentType of t ]

val field_of_lexer : [ `ContentType of Rfc2045.content ] -> field

module D :
sig
  val of_lexer  : Rfc2045.content -> (t, 'r) Decoder.k1
  val of_lexer' : Rfc2045.content -> t
end

module E :
sig
  val w : (field, 'r Encoder.partial) Encoder.k1
end

val equal : t -> t -> bool
val pp    : Format.formatter -> t -> unit
