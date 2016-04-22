val is_safe_char        : char -> bool
val is_hex_octet        : char -> bool

val p_transport_padding : (Lexer.t -> 'a) -> Lexer.t -> 'a

val p_hex_octet         : (char             -> Lexer.t -> 'a) -> Lexer.t -> 'a
val p_ptext             : (string           -> Lexer.t -> 'a) -> Lexer.t -> 'a
val p_qp_section        : (string           -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_qp_segment        : (string           -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_qp_part           : (string           -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_qp_line           : (string list      -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_quoted_printable  : (string list list -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

val p_decode :
  (Lexer.t -> ([< `Continue of Lexer.t | `Stop of Lexer.t  | 'a Lexer.read |
  Lexer.err > `Error ] as 'a))
  -> (string -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret))
  -> Lexer.t -> 'ret
val p_encode :
  (Lexer.t -> ([< `Continue of Lexer.t | `Stop of Lexer.t  | 'a Lexer.read |
  Lexer.err > `Error ] as 'a))
  -> (string -> Lexer.t -> ([> Lexer.err | 'ret Lexer.read ] as 'ret))
  -> Lexer.t -> 'ret
