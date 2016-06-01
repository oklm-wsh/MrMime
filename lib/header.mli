type ('date, 'from) t

type strict   = (Date.t, Address.person list) t
type unstrict = (Date.t option, Address.person list option) t
type phrase   = [ `Phrase of Rfc5322.phrase ]

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

val of_string      : string -> strict
val to_string      : strict -> string

val of_lexer       :
  ('date, 'from) Relax.t ->
  ([> Rfc5322.field ] as 'field) list ->
  (('date, 'from) t -> 'field list -> Decoder.t -> 'a) ->
  Decoder.t -> 'a

val equal          : ('date, 'from) t -> ('date, 'from) t -> bool
val pp             : Format.formatter -> strict -> unit
