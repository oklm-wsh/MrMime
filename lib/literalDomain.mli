type t =
  [ `General of string * string
  | `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t ]

val of_string : string -> t
val pp        : Format.formatter -> t -> unit
val equal     : t -> t -> bool
val size      : t -> int
