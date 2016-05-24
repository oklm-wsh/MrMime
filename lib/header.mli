type t

type phrase = [ `Phrase of Rfc5322.phrase ]

type field =
  [ `From            of Address.person list
  | `Date            of Date.t
  | `Sender          of Address.person
  | `ReplyTo         of Address.List.t
  | `To              of Address.List.t
  | `Cc              of Address.List.t
  | `Bcc             of Address.List.t
  | `Subject         of Rfc5322.phrase
  | `Comments        of Rfc5322.phrase
  | `Keywords        of Rfc5322.phrase list
  | `MessageID       of MsgID.t
  | `InReplyTo       of [ phrase | `MsgID of MsgID.t ] list
  | `References      of [ phrase | `MsgID of MsgID.t ] list
  | `Field           of string * Rfc5322.phrase
  | Resent.field
  | Trace.field
  | `Unsafe          of string * Rfc5322.phrase ]

val field_of_lexer : Rfc5322.field -> field

val of_string : string -> t
val to_string : t -> string
val of_lexer  : ([> Rfc5322.field ] as 'field) list -> (t option -> 'field list -> Lexer.t -> 'a) -> Lexer.t -> 'a

val equal     : t -> t -> bool
val pp        : Format.formatter -> t -> unit
