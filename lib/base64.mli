module type S =
sig
  exception Unexpected_character of char
  exception Wrong_padding

  val encode : Sedlexing.lexbuf -> string
  val encode_buffer : Buffer.t -> Sedlexing.lexbuf -> unit
  val decode : Sedlexing.lexbuf -> string
  val decode_buffer : Buffer.t -> Sedlexing.lexbuf -> unit
end

module Make (S : Lexer.SEDLEXING) : S
