module type A =
sig
  val conv : Buffer.t -> Lexing.lexbuf -> unit
end

module type S =
sig
  val buffer_of_lexbuf : Buffer.t -> Lexing.lexbuf -> unit
  val buffer_of_string : Buffer.t -> string -> unit
  val buffer_of_channel : Buffer.t -> in_channel -> unit
  val buffer_of_fn : Buffer.t -> (string -> int -> int) -> unit

  val string_of_lexbuf : Lexing.lexbuf -> string
  val string_of_string : string -> string
  val string_of_channel : in_channel -> string
  val string_of_fn : (string -> int -> int) -> string
end

module Make (A : A) : S
