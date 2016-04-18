type domain
type local
type mailbox
type person
type group
type t

val pp : Format.formatter -> t -> unit
val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool

module List :
sig
  type nonrec t = t list

  val pp : Format.formatter -> t -> unit
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
end
