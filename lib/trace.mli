type t

type word = [ `Word of Rfc5322.word ]

type received =
  [ word
  | `Domain of Address.domain
  | `Mailbox of Address.mailbox ]

type field =
  [ `Received of received list * Date.t option
  | `ReturnPath of Address.mailbox option ]

val field_of_lexer : Rfc5322.trace -> field
val of_lexer : ([> Rfc5322.trace ] as 'trace) list -> (t option -> 'trace list -> Decoder.t -> 'a) -> Decoder.t -> 'a

val pp       : Format.formatter -> t -> unit
