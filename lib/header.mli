type t

val of_string : string -> t
val to_string : t -> string
val of_lexer  : (t option -> Rfc5322.field list -> 'a) -> Rfc5322.field list -> 'a

val equal     : t -> t -> bool
val pp        : Format.formatter -> t -> unit
