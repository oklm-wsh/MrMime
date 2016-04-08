type t

val make      : string -> string -> t
val pp        : Format.formatter -> t -> unit

val equal     : t -> t -> bool

val of_string : string -> t
val to_string : t -> string
