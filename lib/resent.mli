type t

type field =
  [ `ResentDate      of Date.t
  | `ResentFrom      of Address.person list
  | `ResentSender    of Address.person
  | `ResentTo        of Address.t list
  | `ResentCc        of Address.t list
  | `ResentBcc       of Address.t list
  | `ResentMessageID of MsgID.t
  | `ResentReplyTo   of Address.t list ]

val field_of_lexer  : Rfc5322.resent -> field
val to_field        : t -> field list

val pp_field        : Format.formatter -> field -> unit
val pp              : Format.formatter -> t -> unit

module D :
sig
  val of_lexer : ([> Rfc5322.resent ] as 'resent) list -> (t option, 'resent list, 'r) Decoder.k2
end
