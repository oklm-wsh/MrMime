type t

val to_string : t -> string
val of_string : string -> t
val of_lexer  : Rfc5322.msg_id -> t

val pp        : Format.formatter -> t -> unit
val equal     : t -> t -> bool
