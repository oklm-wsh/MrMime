type t

val make    : int -> int -> t
val default : t
val of_lexer : Rfc2045.version -> t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
