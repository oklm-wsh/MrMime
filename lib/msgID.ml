type t = string * string

let make a b = (a, b)

let to_string (left, right) =
  (* XXX: it's compatible between RFC 822 and RFC 2822:

     RFC 822/addr-spec = local-part "@" domain
     RFC 822/msg-id    = "<" addr-spec ">"

     RFC 2822/msg-id   = [CFWS] "<" id-left "@" id-right ">" [CFWS]
  *)
  "<" ^ left ^ "@" ^ right ^ ">"

let pp fmt i =
  Format.fprintf fmt "%s" (to_string i)

let equal (a, b) (x, y) =
  (a = x) && (b = y)

let of_string str =
  try Rfc822.msg_id (Lexing.from_string str)
  with exn ->
    Rfc2822.msg_id (Lexing.from_string str)
