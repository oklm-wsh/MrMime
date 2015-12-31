exception Unexpected_character of char
exception Wrong_padding

val encode : Lexing.lexbuf -> string
val encode_buffer : Buffer.t -> Lexing.lexbuf -> unit
val decode : Lexing.lexbuf -> string
val decode_buffer : Buffer.t -> Lexing.lexbuf -> unit
