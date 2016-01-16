type t

val make    : int -> int -> t
val default : t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
