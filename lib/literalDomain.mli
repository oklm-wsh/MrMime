type t =
  [ `General of string * string
  | `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t ]

module D :
sig
  val of_decoder : Decoder.t -> t
end

module E :
sig
  val w : (t, 'r Encoder.partial) Wrap.k1
  val to_buffer : t -> Encoder.t -> Buffer.t
end

val to_string : t -> string
val of_string : string -> t

val pp        : Format.formatter -> t -> unit
val equal     : t -> t -> bool

val size      : t -> int
