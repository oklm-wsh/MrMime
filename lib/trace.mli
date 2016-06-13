type word = [ `Word of Rfc5322.word ]

type received =
  [ word
  | `Domain of Address.domain
  | `Mailbox of Address.mailbox ]

type t

type field =
  [ `Received of received list * Date.t option
  | `ReturnPath of Address.mailbox option ]

val field_of_lexer : Rfc5322.trace -> field
val to_field       : t -> field list

module D :
sig
  val of_lexer : ([> Rfc5322.trace ] as 'trace) list -> (t option, 'trace list, 'r) Decoder.k2
end
