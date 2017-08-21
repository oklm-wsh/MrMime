type raw = Rfc2047.raw =
  | QuotedPrintable of string
  | Base64 of Base64.Decoder.result
type unstructured     =
  [ `CR of int
  | `CRLF
  | `Encoded of string * raw
  | `LF of int
  | `Text of string
  | `WSP ] list
type phrase_or_msg_id =
  [ `MsgID of MsgID.msg_id
  | `Phrase of
      [ `Dot
      | `Encoded of string * raw
      | `Word of [ `String of string | `Atom of string ] ] list ]
type field = Rfc5322.field

module Map : (module type of Map.Make(String))

type header =
  { date        : Date.date option
  ; from        : Address.mailbox list
  ; sender      : Address.mailbox option
  ; reply_to    : Address.address list
  ; to'         : Address.address list
  ; cc          : Address.address list
  ; bcc         : Address.address list
  ; subject     : unstructured option
  ; msg_id      : MsgID.msg_id option
  ; in_reply_to : phrase_or_msg_id list
  ; references  : phrase_or_msg_id list
  ; comments    : unstructured list
  ; keywords    : Address.phrase list list
  ; resents     : Resent.resent list
  ; traces      : Trace.trace list
  ; fields      : unstructured list Map.t
  ; unsafe      : unstructured list Map.t
  ; skip        : string list }

val pp_raw              : Format.formatter -> raw -> unit
val pp_unstructured     : Format.formatter -> unstructured -> unit
val pp_phrase_or_msg_id : Format.formatter -> phrase_or_msg_id -> unit
val pp_field            : Format.formatter -> field -> unit
val pp                  : Format.formatter -> header -> unit

module Encoder :
sig
  val w_unstructured : (unstructured, 'r Encoder.partial) Wrap.k1
  val w_field        : ([ Rfc5322.field_header | Rfc5322.skip ], 'r Encoder.partial) Encoder.k1
  val w_header       : (header, 'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  val header : ([> field ] as 'a) list -> (header * 'a list) Parser.t
end

val to_string           : header -> string
val of_string           : ?chunk:int -> string -> (header * [> field ] list) option
val of_string_raw       : ?chunk:int -> string -> int -> int -> ((header * [> field ] list) * int) option

val equal               : header -> header -> bool
