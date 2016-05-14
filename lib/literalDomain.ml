open BasePrinter

type t =
  [ `General of string * string
  | `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t ]

let of_string s =
  let rec loop = function
    | `Error (exn, buf, off, len) ->
      let tmp = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer tmp in

      Format.fprintf fmt "%a (buf: %S)%!"
        Error.pp exn (Bytes.sub buf off (len - off));

      raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
    | `Read (buf, off, len, k) ->
      raise (Invalid_argument "Address.of_string: unterminated string")
    | `Ok data -> data
  in

  let rule = Rfc5321.p_address_literal
    (fun data -> Rfc5322.p_crlf (fun _ -> `Ok data)) in
  loop @@ BaseLexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n"))

let p = Format.fprintf

let pp fmt = function
  | `General (tag, content) ->
    p fmt "%a:%a"
      (pp_string ~in_qs:false ~in_dm:true) tag
      (pp_string ~in_qs:false ~in_dm:true) content
  | `IPv4 ip -> Ipaddr.V4.pp_hum fmt ip
  | `IPv6 ip -> p fmt "IPv6:%a" Ipaddr.V6.pp_hum ip

let equal = (=)

let size domain =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in

  Format.fprintf fmt "%a%!" pp domain;
  Buffer.length buf
