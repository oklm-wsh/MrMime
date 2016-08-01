open Parser

val is_hex          : char -> bool

val hex             : char t
val decode          : unit t -> unit t -> string t
val inline          : unit -> string t

module Convenience :
sig
  module Input : module type of RingBuffer.Committed
    with type 'a t = 'a RingBuffer.Committed.t

  type 'a decoder
  type decoding =
    [ `Continue
    | `Error of err
    | `Dirty of char
    | `End of string
    | `String of string ]

  val decoder       : (unit Parser.t * unit Parser.t) -> 'a Input.t -> 'a decoder
  val decoder_src   : 'a decoder -> 'a Input.t

  val decode        : 'a decoder -> decoding
  val src           : 'a decoder -> string -> int -> int -> unit
end

val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
val w_encode        : string -> ('r Encoder.partial) Encoder.k0
