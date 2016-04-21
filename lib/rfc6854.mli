open Rfc5322

val p_from            : (address list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_sender          : (address      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_reply_to        : (address list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

val p_resent_from     : (address list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_resent_sender   : (address      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
