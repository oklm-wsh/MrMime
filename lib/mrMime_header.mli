type raw              = Rfc2047.raw = QuotedPrintable of string | Base64 of MrMime_base64.result
type unstructured     =
  [ `CR of int
  | `CRLF
  | `Encoded of string * raw
  | `LF of int
  | `Text of string
  | `WSP ] list
type phrase_or_msg_id =
  [ `MsgID of MrMime_msgID.msg_id
  | `Phrase of
      [ `Dot
      | `Encoded of string * raw
      | `Word of [ `String of string | `Atom of string ] ] list ]
type field            = Rfc5322.field

module Map      : (module type of Map.Make(String))

type header =
  { date        : MrMime_date.date option
  ; from        : MrMime_address.mailbox list
  ; sender      : MrMime_address.mailbox option
  ; reply_to    : MrMime_address.address list
  ; to'         : MrMime_address.address list
  ; cc          : MrMime_address.address list
  ; bcc         : MrMime_address.address list
  ; subject     : unstructured option
  ; msg_id      : MrMime_msgID.msg_id option
  ; in_reply_to : phrase_or_msg_id list
  ; references  : phrase_or_msg_id list
  ; comments    : unstructured list
  ; keywords    : MrMime_address.phrase list list
  ; resents     : MrMime_resent.resent list
  ; traces      : MrMime_trace.trace list
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
  val header : ([> field ] as 'a) list -> (header * 'a list) MrMime_parser.t
end

val to_string           : header -> string
val of_string           : ?chunk:int -> string -> (header * [> field ] list) option
val of_string_raw       : ?chunk:int -> string -> int -> int -> ((header * [> field ] list) * int) option

val equal               : header -> header -> bool
