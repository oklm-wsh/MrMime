type t

val of_lexer : ([> Rfc5322.resent ] as 'resent) list -> (t option -> 'resent list -> Lexer.t -> 'a) -> Lexer.t -> 'a

val pp       : Format.formatter -> t -> unit
