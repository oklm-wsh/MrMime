type t

type 'r           k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)     k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

val string        : (string, 'r Encoder.partial) k1
val close_box     : 'r Encoder.partial k0
val hbox          : 'r Encoder.partial k0
val vbox          : (int, 'r Encoder.partial) k1
val hvbox         : (int, 'r Encoder.partial) k1
val hovbox        : (int, 'r Encoder.partial) k1
val box           : (int, 'r Encoder.partial) k1
val newline       : 'r Encoder.partial k0
val flush         : 'r Encoder.partial k0
val force_newline : 'r Encoder.partial k0
val if_newline    : 'r Encoder.partial k0
val break         : (int, int, 'r Encoder.partial) k2
val space         : 'r Encoder.partial k0
val cut           : 'r Encoder.partial k0
val char          : (char, 'r Encoder.partial) k1
val limit         : (int, 'r Encoder.partial) k1

val lift          : ?margin:int -> (t -> 'a) -> Encoder.t -> 'a
val unlift        : (Encoder.t -> 'r Encoder.partial) -> t -> 'r Encoder.partial
