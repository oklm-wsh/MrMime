type t =
  [ `General of string * string
  | `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t ]

module D =
struct
  open BaseDecoder

  let of_decoder state =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Error.pp exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("LiteralDomain.of_string: " ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Address.of_string unterminated string")
      | `Ok data -> data
    in

    let rule =
      Rfc5321.p_address_literal
      @ fun data -> Rfc822.p_crlf
      @ Rfc822.p_crlf
      @ fun _ -> `Ok data
    in

    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let explode str =
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (str.[i] :: l) in
      exp (String.length str - 1) []

    let w_safe_string str =
      List.fold_right
        (function
         | '\x00' -> w_string "\\\000"
         | '\x07' -> w_string "\\a"
         | '\x08' -> w_string "\\b"
         | '\x09' -> w_string "\\t"
         | '\x0A' -> w_string "\\n"
         | '\x0B' -> w_string "\\v"
         | '\x0C' -> w_string "\\f"
         | '\x0D' -> w_string "\\r"
         | '\\'   -> w_string "\\\\"
         | '"'    -> w_string "\\\""
         | ' '    -> w_string " "
         | ']'    -> w_string "\\]"
         | chr    ->
           if Rfc5322.is_vchar chr
           then w_char chr
           else w_string (sp "\\%c" chr))
        (explode str)

    let w_domain = function
      | `General (tag, value) ->
        w_hovbox 1
        $ w_string "["
        $ w_string tag
        $ w_string ":"
        $ w_safe_string value
        $ w_string "]"
        $ w_close_box
      | `IPv4 ipv4 ->
        w_hovbox 1
        $ w_string "["
        $ w_string (Ipaddr.V4.to_string ipv4)
        $ w_string "]" $ w_close_box
      | `IPv6 ipv6 ->
        w_hovbox 1
        $ w_string "["
        $ w_string "IPv6"
        $ w_string ":"
        $ w_string (Ipaddr.V6.to_string ipv6)
        $ w_string "]"
        $ w_close_box
  end

  let w = Internal.w_domain

  let to_buffer t state =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> buf
    in

    let rule =
      let open BaseEncoder in
      let ok = flush (fun _ -> `Ok) in
      Wrap.lift Wrap.(Internal.w_domain t (unlift ok))
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string (s ^ "\r\n\r\n"))
let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

let pp fmt _ = Format.fprintf fmt "#literal-domain"
let equal = (=)

let size domain =
  String.length @@ to_string domain
