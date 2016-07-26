type word   = [ `Atom of string | `String of string ]
type local  = word list
type domain = [ `Domain of string list | `Literal of string ]
type msg_id = (local * domain)

val pp_local      : Format.formatter -> local -> unit
val pp_domain     : Format.formatter -> domain -> unit
val pp            : Format.formatter -> msg_id -> unit

module Encoder :
sig
  val w_left      : (local, 'r Encoder.partial) Wrap.k1
  val w_right     : (domain, 'r Encoder.partial) Wrap.k1
  val w_msg_id    : (msg_id, 'r Encoder.partial) Wrap.k1
end

module Decoder :
sig
  val p_msg_id    : msg_id MrMime_parser.t
end

val of_string     : ?chunk:int -> string -> msg_id option
val of_string_raw : ?chunk:int -> string -> int -> int -> (msg_id * int) option
