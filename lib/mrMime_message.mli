type field_message = Top.field_message
type field_part    = Top.field_part

type 'a message = 'a Top.message =
  | Discrete  of MrMime_content.t * field_message list * 'a
  | Extension of MrMime_content.t * field_message list
  | Composite of MrMime_content.t * field_message list * (MrMime_content.t * field_part list * 'a part option) list
and 'a part = 'a Top.part =
  | PDiscrete  of 'a
  | PExtension of MrMime_content.t * field_part list
  | PComposite of (MrMime_content.t * field_part list * 'a part option) list

type content = ..
type content += Base64 of MrMime_base64.result
type content += QuotedPrintable of string
type content += Raw of string

module Decoder :
sig
  val p_message   : (MrMime_header.header * content message) MrMime_parser.t
end

val of_string_raw : ?chunk:int -> string -> int -> int -> ((MrMime_header.header * content message) * int) option
