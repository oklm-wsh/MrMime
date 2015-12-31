type t =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `Quoted_printable
  | `Ietf_token of string
  | `X_token of string ]

(** See RFC 2045 § 6.1:

    This is the default value -- that is,  "Content-Transfer-Encoding:  7BIT" is
    assumed  if  the  Content-Transfer-Encoding  header  field  is  not present.
*)
let default = `Bit7

let to_string = function
  | `Base64 -> "base64"
  | `Bit7 -> "7bit"
  | `Bit8 -> "8bit"
  | `Binary -> "binary"
  | `Quoted_printable -> "quoted-printable"
  | `Ietf_token s | `X_token s -> s

(** See RFC 2045 § 6.1:

    These values are not case sensitive --  Base64 and BASE64 and bAsE64 are all
    equivalent.
*)
let of_string str =
  (* XXX: Why Utf8 ? *)
  try  Lexer.mechanism (Lexing.from_string (String.lowercase str))
  with Invalid_argument "Lexer.mechanism" ->
         raise (Invalid_argument "Encoding.of_string")
