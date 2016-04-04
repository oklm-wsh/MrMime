type t = string * string

let to_string (left, right) =
  (* XXX: it's compatible between RFC 822 and RFC 2822:

     RFC 822/addr-spec = local-part "@" domain
     RFC 822/msg-id    = "<" addr-spec ">"

     RFC 2822/msg-id   = [CFWS] "<" id-left "@" id-right ">" [CFWS]
  *)
  "<" ^ left ^ "@" ^ right ^ ">"

let of_string str =
  try Lexer.rfc2822_msg_id None (Lexing.from_string str)
  with Invalid_argument "Lexer.msg_id" ->
    try Lexer.rfc2045_msg_id (Lexing.from_string str)
    with Invalid_argument "Lexer.msg_id" ->
      raise (Invalid_argument "MsgID.of_string")
