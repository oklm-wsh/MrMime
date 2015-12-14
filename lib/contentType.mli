type t

val ty : t -> string
val subty : t -> string
val parameters : t -> (string * string) list

val make : ?parameters:(string * string) list -> string -> string -> t
val default : t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool
