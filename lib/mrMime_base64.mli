open Parser

type result = [ `Dirty of string | `Clean of string | `Wrong_padding ]

val is_b64          : char -> bool

val decode          : unit t -> unit t -> result t
val inline          : unit -> result t

module Convenience :
sig
  type err += Wrong_padding

  type 'a decoder
  type decode =
    [ `Continue
    | `Error of err
    | `Dirty of string
    | `End of string
    | `String of string ]

  val decoder       : (unit Parser.t * unit Parser.t) -> 'a Input.t -> 'a decoder
  val decoder_src   : 'a decoder -> 'a Input.t

  val decode        : 'a decoder -> decode
  val src           : 'a decoder -> string -> int -> int -> unit
end

val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
val w_encode        : string -> ('r Encoder.partial) Encoder.k0
