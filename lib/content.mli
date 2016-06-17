type t

val ty : t -> ContentType.t
val encoding : t -> ContentEncoding.t

module Part :
sig
  type field =
    [ `ContentType        of ContentType.t
    | `ContentEncoding    of ContentEncoding.t
    | `ContentID          of MsgID.t
    | `ContentDescription of Rfc5322.phrase
    | `Content            of string * Rfc5322.phrase
    | `Unsafe             of string * Rfc5322.phrase ]

  val field_of_lexer : Rfc2045.field -> field
  val to_field       : t -> field list

  val pp_field       : Format.formatter -> field -> unit

  module D :
  sig
    val of_lexer : ([> Rfc2045.field ] as 'content) list -> (t, 'content list, 'r) Decoder.k2
  end

  module E :
  sig
    val w_field : (field, 'r Encoder.partial) Encoder.k1
    val w : (field list, 'r Encoder.partial) Encoder.k1
  end
end

module Message :
sig
  type field = [ Part.field | MimeVersion.field ]

  val field_of_lexer : [ Rfc2045.field | Rfc2045.mime_field ] -> field
  val to_field       : t -> field list

  module D :
  sig
    val of_lexer : ([> Rfc2045.field | Rfc2045.mime_field ] as 'content) list -> (t, 'content list, 'r) Decoder.k2
  end

  module E :
  sig
    val w_field : (field, 'r Encoder.partial) Encoder.k1
    val w : (field list, 'r Encoder.partial) Encoder.k1
  end
end

val pp : Format.formatter -> t -> unit
