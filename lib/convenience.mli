type 'a decoding =
  [ `Continue
  | `Error of Parser.err
  | `Done of 'a ]

type ('input, 'a) decoder

val decoder_src      : ('input, 'a) decoder -> 'input Input.t
val decoder          : 'input Input.t -> 'a Parser.t -> ('input, 'a) decoder
val decode           : ('input, 'a) decoder -> 'a decoding
val src              : ('input, 'a) decoder -> bytes -> int -> int -> unit
val decoding         : ('input, 'a) decoder -> 'b Parser.t -> ('input, 'b) decoder
