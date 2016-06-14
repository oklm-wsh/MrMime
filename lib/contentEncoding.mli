type t =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Ietf_token of string
  | `X_token of string ]

val default : t

type field = [ `ContentEncoding of t ]

val field_of_lexer : [ `ContentEncoding of t ] -> field

module D :
sig
  val of_lexer  : Rfc2045.encoding -> (t, 'r) Decoder.k1
  val of_lexer' : Rfc2045.encoding -> t
end

module E :
sig
  val w : (field, 'r Encoder.partial) Encoder.k1
end

val to_string : t -> string

val equal : t -> t -> bool
val pp    : Format.formatter -> t -> unit
val pp_field : Format.formatter -> field -> unit
