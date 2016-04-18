type t

val pp : Format.formatter -> t -> unit
val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
