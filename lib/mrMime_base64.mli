open Parser

type result = [ `Dirty of string | `Clean of string | `Wrong_padding ]

val is_b64 : char -> bool

val decode : unit t -> unit t -> result t
val inline : unit -> result t

val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
val w_encode        : string -> ('r Encoder.partial) Encoder.k0
