open BaseDecoder

type literal_domain =
  [ `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t
  | `General of (string * string) ]

let p_ipv4_address_literal p state =
  let pos = ref state.Decoder.pos in

  try let ip = Ipaddr.V4.of_string_raw state.Decoder.buffer pos in

      state.Decoder.pos <- !pos;
      p ip state
  with exn -> raise (Error.Error (Error.err_invalid_ipv4 state))

let p_ipv6_addr p state =
  let pos = ref state.Decoder.pos in

  try
    let ip  = Ipaddr.V6.of_string_raw state.Decoder.buffer pos in

    state.Decoder.pos <- !pos;
    p ip state
  with exn -> raise (Error.Error (Error.err_invalid_ipv6 state))

let p_ipv6_address_literal p =
  p_str "IPv6:"
  @ p_ipv6_addr p

let p_ldh_str p =
  p_while
    (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false)
  @ fun s ->
    if String.get s (String.length s - 1) = '-'
    then fun state -> raise (Error.Error (Error.err_unexpected '-' state))
    else p s

let is_dcontent = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | _ -> false

let p_general_address_literal p =
  p_ldh_str
  @ fun tag -> p_chr ':'
  @ p_while is_dcontent
  @ fun content ->
    (* XXX:  we already try to parse IPv6 tag, so if we are in this
             case, we have an invalid IPv6 data.
       TODO: may be it's useful to associate a Decoder with the tag
             and try this. I have no time for that.
    *)
    if Iana.Set.exists ((=) tag) Iana.tag && tag <> "IPv6"
    then p (tag, content)
    else if tag = "IPv6"
    then fun state -> raise (Error.Error (Error.err_invalid_ipv6 state))
    else fun state -> raise (Error.Error (Error.err_invalid_tag tag state))

let p_address_literal p =
  (p_ipv4_address_literal (fun v4 state -> `Ok (v4, state)))
  / ((p_ipv6_address_literal (fun v6 state -> `Ok (v6, state)))
     / (p_general_address_literal
        @ fun (tag, content) -> p (`General (tag, content)))
     @ (fun ipv6 -> p (`IPv6 ipv6)))
  @ (fun ipv4 -> p (`IPv4 ipv4))

