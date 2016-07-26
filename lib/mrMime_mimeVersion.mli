type version = (int * int)
type field   = [ `MimeVersion of version ]

val pp            : Format.formatter -> version -> unit

val default       : version

module Encoder :
sig
  val w_version   : (version, 'r Encoder.partial) Wrap.k1
  val w_field     : (field, 'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  val p_version   : version MrMime_parser.t
end

val of_string     : ?chunk:int -> string -> version option
val of_string_raw : ?chunk:int -> string -> int -> int -> (version * int) option
