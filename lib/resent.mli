type t

type field =
  [ `ResentDate of Date.t
  | `ResentFrom of Address.person list
  | `ResentSender of Address.person
  | `ResentTo of Address.List.t
  | `ResentCc of Address.List.t
  | `ResentBcc of Address.List.t
  | `ResentMessageID of MsgID.t
  | `ResentReplyTo of Address.List.t ]

val field_of_lexer : Rfc5322.resent -> field
val of_lexer : ([> Rfc5322.resent ] as 'resent) list -> (t option -> 'resent list -> Lexer.t -> 'a) -> Lexer.t -> 'a

val pp       : Format.formatter -> t -> unit
