type local  = Rfc822.local
type domain = Rfc822.domain
type msg_id = Rfc822.msg_id

val pp_local      : Format.formatter -> local -> unit
val pp_domain     : Format.formatter -> domain -> unit
val pp            : Format.formatter -> msg_id -> unit

module Encoder :
sig
  val w_left      : (local, 'r Encoder.partial) Wrap.k1
  val w_right     : (domain, 'r Encoder.partial) Wrap.k1
  val w_msg_id    : (msg_id, 'r Encoder.partial) Wrap.k1
end

val of_string     : ?chunk:int -> string -> msg_id option
val of_string_raw : ?chunk:int -> string -> int -> int -> (msg_id * int) option
