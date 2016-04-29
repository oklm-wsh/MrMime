type t =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Ietf_token of string
  | `X_token of string ]

(** See RFC 2045 § 6.1:

    This is the default value -- that is,  "Content-Transfer-Encoding:  7BIT" is
    assumed  if  the  Content-Transfer-Encoding  header  field  is  not present.
*)
let default = `Bit7

let of_lexer x = x

let to_string = function
  | `Base64 -> "base64"
  | `Bit7 -> "7bit"
  | `Bit8 -> "8bit"
  | `Binary -> "binary"
  | `QuotedPrintable -> "quoted-printable"
  | `Ietf_token s | `X_token s -> s

let p = Format.fprintf

let pp fmt = function
  | `Base64 -> p fmt "base64"
  | `Bit7   -> p fmt "7bit"
  | `Bit8   -> p fmt "8bit"
  | `Binary -> p fmt "binary"
  | `QuotedPrintable -> p fmt "quoted-printable"
  | `Ietf_token s -> p fmt "%s" s
  | `X_token s -> p fmt "%s" s
