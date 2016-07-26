open Parser

val is_hex : char -> bool

val hex    : char t
val decode : unit t -> unit t -> string t
val inline : unit -> string t

val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
val w_encode        : string -> ('r Encoder.partial) Encoder.k0
