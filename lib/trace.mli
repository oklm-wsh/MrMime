type t

val of_lexer : Rfc5322.field list -> (t option -> Rfc5322.field list -> Lexer.t -> 'a) -> Lexer.t -> 'a

val pp       : Format.formatter -> t -> unit
