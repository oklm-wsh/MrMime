type t =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Ietf_token of string
  | `X_token of string ]

let pp = Format.fprintf

let pp fmt = function
  | `Binary          -> pp fmt "binary"
  | `Ietf_token x    -> pp fmt "ietf-token:%s" x
  | `Base64          -> pp fmt "base64"
  | `X_token x       -> pp fmt "x-token:%s" x
  | `Bit7            -> pp fmt "7bit"
  | `QuotedPrintable -> pp fmt "quoted-printable"
  | `Bit8            -> pp fmt "8bit"

(** See RFC 2045 § 6.1:

    This is the default value -- that is,  "Content-Transfer-Encoding:  7BIT" is
    assumed  if  the  Content-Transfer-Encoding  header  field  is  not present.
*)
let default = `Bit7

type field = [ `ContentEncoding of t ]

let field_of_lexer x = x

let to_string = function
  | `Base64 -> "base64"
  | `Bit7 -> "7bit"
  | `Bit8 -> "8bit"
  | `Binary -> "binary"
  | `QuotedPrintable -> "quoted-printable"
  | `Ietf_token s | `X_token s -> s

let pp_field fmt = function
  | `ContentEncoding t ->
    Format.fprintf fmt "@[<hov>Content-Encoding = %a@]" pp t

module D =
struct
  let of_lexer x p state = p x state
  let of_lexer' x = x
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder

    let w_encoding x = w (to_string x)

    let w_crlf k e = w "\r\n" k e

    let wrap a =
      let buf = Buffer.create 16 in

      let rec loop = function
        | `Partial (s, i, l, k) ->
          Buffer.add_subbytes buf s i l;
          loop @@ (k l)
        | `Ok ->
          Wrap.w_string (Buffer.contents buf)
      in

      loop @@ (a (flush (fun _ -> `Ok)) (Encoder.make ()))

    let w_field = function
      | `ContentEncoding x ->
        w "Content-Transfer-Encoding: "
        $ Wrap.lift
        $ Wrap.(fun k -> (w_hovbox (String.length "Content-Transfer-Encoding: ")
                          $ wrap (w_encoding x)
                          $ w_close_box) (unlift k))
        $ w_crlf
  end

  let w = Internal.w_field
end

let equal = (=)
