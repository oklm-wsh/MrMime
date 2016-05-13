type t

type field = [ `MimeVersion of (int * int) ]

val field_of_lexer : Rfc2045.mime_field -> field

val make    : int -> int -> t
val default : t
val of_lexer : Rfc2045.mime_field -> t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
