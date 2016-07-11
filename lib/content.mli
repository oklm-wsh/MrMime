module Map          : (module type of Map.Make(String))

type unstructured   = Rfc5322.unstructured
type field          = [ Rfc2045.field | Rfc2045.field_version | Rfc2045.skip ]

type t =
  { ty              : ContentType.content
  ; encoding        : ContentEncoding.mechanism
  ; version         : MimeVersion.version
  ; id              : MsgID.msg_id option
  ; description     : unstructured option
  ; content         : unstructured list Map.t
  ; unsafe          : unstructured list Map.t
  ; skip            : string list }

val pp_raw          : Format.formatter -> Rfc2047.raw -> unit
val pp_unstructured : Format.formatter -> unstructured -> unit
val pp_field        : Format.formatter -> field -> unit
val pp              : Format.formatter -> t -> unit

val default         : t

module Encoder :
sig
  val w_field         : (Rfc2045.field, 'r Encoder.partial) Encoder.k1
  val w_field_version : (Rfc2045.field_version, 'r Encoder.partial) Encoder.k1
  val w_unsafe        : (Rfc2045.unsafe, 'r Encoder.partial) Encoder.k1
  val w_skip          : (Rfc2045.skip, 'r Encoder.partial) Encoder.k1

  val w_message       : (t, 'r Encoder.partial) Encoder.k1
  val w_part          : (t, 'r Encoder.partial) Encoder.k1
end

val message         : ([> Rfc2045.field | Rfc2045.field_version ] as 'a) list -> (t * 'a list) Parser.t
val part            : ([> Rfc2045.field | Rfc2045.unsafe | Rfc2045.skip ] as 'a) list -> (t * 'a list) Parser.t
