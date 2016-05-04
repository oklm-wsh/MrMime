type t

val of_lexer : ([> Rfc5322.trace ] as 'trace) list -> (t option -> 'trace list -> Lexer.t -> 'a) -> Lexer.t -> 'a

val pp       : Format.formatter -> t -> unit
