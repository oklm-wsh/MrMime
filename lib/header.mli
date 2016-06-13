type ('date, 'from) t
type strict   = (Date.t, Address.person list) t
type unstrict = (Date.t option, Address.person list option) t

module Relax :
sig
  type ('date, 'from) t

  exception Expected_date
  exception Expected_from

  val strict   : (Date.t, Address.person list) t
  val unstrict : (Date.t option, Address.person list option) t
end

type field =
  [ `From            of Address.person list
  | `Date            of Date.t
  | `Sender          of Address.person
  | `ReplyTo         of Address.t list
  | `To              of Address.t list
  | `Cc              of Address.t list
  | `Bcc             of Address.t list
  | `Subject         of Rfc5322.phrase
  | `Comments        of Rfc5322.phrase
  | `Keywords        of Rfc5322.phrase list
  | `MessageID       of MsgID.t
  | `InReplyTo       of [ `Phrase of Rfc5322.phrase | `MsgID of MsgID.t ] list
  | `References      of [ `Phrase of Rfc5322.phrase | `MsgID of MsgID.t ] list
  | `Field           of string * Rfc5322.phrase
  | Resent.field
  | Trace.field
  | `Unsafe          of string * Rfc5322.phrase ]

val field_of_lexer : Rfc5322.field -> field
val to_field       : unstrict -> field list

module D :
sig
  val of_lexer     : ('date, 'from) Relax.t -> ([> Rfc5322.field ] as 'field) list -> (('date, 'from) t, 'field list, 'r) Decoder.k2
  val of_decoder   : Decoder.t -> unstrict
end

module E :
sig
  val w            : (field list, 'r Encoder.partial) Encoder.k1
  val to_buffer    : unstrict -> Encoder.t -> Buffer.t
end

val of_string      : string -> unstrict
val to_string      : unstrict -> string

val equal          : ('date, 'from) t -> ('date, 'from) t -> bool
val pp             : Format.formatter -> ('date, 'from) t -> unit
