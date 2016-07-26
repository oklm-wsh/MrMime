type mechanism = Rfc2045.mechanism
type field     = [ `ContentEncoding of mechanism ]

(* convenience alias *)
module Input = MrMime_input

let pp = Format.fprintf

let pp fmt = function
  | `Bit7            -> pp fmt "7bit"
  | `Bit8            -> pp fmt "8bit"
  | `Binary          -> pp fmt "binary"
  | `QuotedPrintable -> pp fmt "quoted-printable"
  | `Base64          -> pp fmt "base64"
  | `Ietf_token s    -> pp fmt "ietf:\"%s\"" s
  | `X_token s       -> pp fmt "x:\"%s\"" s

let default = `Bit7

module Encoder =
struct
  open Encoder

  let to_string = function
    | `Bit7 -> "7bit"
    | `Bit8 -> "8bit"
    | `Binary -> "binary"
    | `QuotedPrintable -> "quoted-printable"
    | `Base64 -> "base64"
    | `Ietf_token s | `X_token s -> s

  let w_encoding x = string (to_string x)
  let w_crlf k e = string "\r\n" k e

  let wrap a =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok ->
        Wrap.string (Buffer.contents buf)
    in

    loop @@ (a (flush (fun _ -> `Ok)) (Encoder.make ()))

  let w_field = function
    | `ContentEncoding x ->
      string "Content-Transfer-Encoding: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ wrap (w_encoding x) $ close_box) (unlift k))))
      $ w_crlf
end

module Decoder =
struct
  let p_encoding = Rfc2045.encoding
end

let of_string ?(chunk = 1024) s =
  let s' = s ^ "\r\n" in
  let l = String.length s' in
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (l - consumed) in
      Input.write_string buffer s' consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  aux 0 @@ Parser.run i Parser.(Rfc2045.encoding <* Rfc822.crlf)

let of_string_raw ?(chunk = 1024) s off len =
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (len - (consumed - off)) in
      Input.write_string buffer s consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some (v, consumed - off)
  in

  aux off @@ Parser.run i Rfc2045.encoding
