type field = Grammar.field

type t = Header.unstrict *
  [ `Composite of Content.t * nest option list
  | `Discrete of Content.t * string ]
and nest =
  [ `Composite of (Content.t * field list) * 'a option list
  | `Discrete of (Content.t * field list) * string ] as 'a

module D :
sig
  val of_decoder : Decoder.t -> t
end

module E :
sig
  val w : (t, 'r Encoder.partial) Encoder.k1
  val to_buffer : t -> Encoder.t -> Buffer.t
end

val of_string : string -> t
val to_string : t -> string

val equal : t -> t -> bool
val pp    : Format.formatter -> t -> unit
