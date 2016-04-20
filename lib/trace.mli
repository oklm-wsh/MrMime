type t

val of_lexer : (t option -> Rfc5322.field list -> 'a) -> Rfc5322.field list -> 'a

val pp       : Format.formatter -> t -> unit
