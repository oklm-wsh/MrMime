type t =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Ietf_token of string
  | `X_token of string ]

val default : t
val to_string : t -> string
val of_lexer : Rfc2045.encoding -> t
val pp : Format.formatter -> t -> unit
