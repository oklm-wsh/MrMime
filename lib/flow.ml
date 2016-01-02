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

module Make (A : A) : S =
struct
  let buffer_of_lexbuf buf lexbuf =
    A.conv buf lexbuf

  let buffer_of_string buf data =
    A.conv buf (Lexing.from_string data)

  let buffer_of_channel buf ch =
    A.conv buf (Lexing.from_channel ch)

  let buffer_of_fn buf fn =
    A.conv buf (Lexing.from_function fn)

  let string_of_lexbuf lexbuf =
    let buffer = Buffer.create 16 in
    buffer_of_lexbuf buffer lexbuf; Buffer.contents buffer

  let string_of_string data =
    let buffer = Buffer.create 16 in
    buffer_of_string buffer data; Buffer.contents buffer

  let string_of_channel ch =
    let buffer = Buffer.create 16 in
    buffer_of_channel buffer ch; Buffer.contents buffer

  let string_of_fn fn =
    let buffer = Buffer.create 16 in
    buffer_of_fn buffer fn; Buffer.contents buffer
end
