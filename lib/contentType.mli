type ty    = Rfc2045.ty
type subty = Rfc2045.subty
type value = Rfc2045.value
type t

val ty         : t -> ty
val subty      : t -> subty
val parameters : t -> (string * Rfc2045.value) list

val make : ?parameters:(string * value) list -> ty -> subty -> t
val default : t
val of_lexer : Rfc2045.content -> t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
