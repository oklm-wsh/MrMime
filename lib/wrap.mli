type t
type 'r           k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)     k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

val w_string        : (string, 'r Encoder.partial) k1
val w_close_box     : 'r Encoder.partial k0
val w_hbox          : 'r Encoder.partial k0
val w_vbox          : (int, 'r Encoder.partial) k1
val w_hvbox         : (int, 'r Encoder.partial) k1
val w_hovbox        : (int, 'r Encoder.partial) k1
val w_box           : (int, 'r Encoder.partial) k1
val w_newline       : 'r Encoder.partial k0
val w_flush         : 'r Encoder.partial k0
val w_force_newline : 'r Encoder.partial k0
val w_if_newline    : 'r Encoder.partial k0
val w_break         : (int, int, 'r Encoder.partial) k2
val w_space         : 'r Encoder.partial k0
val w_cut           : 'r Encoder.partial k0
val w_char          : (char, 'r Encoder.partial) k1
val w_limit         : (int, 'r Encoder.partial) k1

val lift            : ?margin:int -> (t -> 'a) -> Encoder.t -> 'a
val unlift          : (Encoder.t -> 'r Encoder.partial) -> t -> 'r Encoder.partial
