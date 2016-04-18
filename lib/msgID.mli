type t

val make      : string -> string -> t
val pp        : Format.formatter -> t -> unit

val equal     : t -> t -> bool

val to_string : t -> string
