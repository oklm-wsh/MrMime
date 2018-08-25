open Parser

type err += Invalid_charset

type raw =
  | QuotedPrintable of string
  | Base64 of Base64.Decoder.result

val is_ctl   : char -> bool
val is_space : char -> bool

val token                 : string t
val inline_encoded_string : (string * raw) t
