type literal_domain =
  [ `IPv4 of Ipaddr.V4.t
  | `IPv6 of Ipaddr.V6.t
  | `General of (string * string) ]

let p_ipv4_address_literal p state =
  (Logs.debug @@ fun m -> m "state: p_ipv4_address_literal");

  let pos = ref state.Lexer.pos in

  try let ip = Ipaddr.V4.of_string_raw state.Lexer.buffer pos in

      state.Lexer.pos <- !pos;
      p ip state
  with exn -> raise (Lexer.Error (Lexer.err_invalid_ipv4 state))

let p_ipv6_addr p state =
  (Logs.debug @@ fun m -> m "state: p_ipv6_addr");

  let pos = ref state.Lexer.pos in

  try
    let ip  = Ipaddr.V6.of_string_raw state.Lexer.buffer pos in

    state.Lexer.pos <- !pos;
    p ip state
  with exn -> raise (Lexer.Error (Lexer.err_invalid_ipv6 state))

let p_ipv6_address_literal p state =
  let _ = Lexer.p_str "IPv6:" state in

  p_ipv6_addr p state

let p_ldh_str p state =
  let s = Lexer.p_while
    (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false)
    state
  in

  if String.get s (String.length s - 1) = '-'
  then raise (Lexer.Error (Lexer.err_unexpected '-' state))
  else p s state

let is_dcontent = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | _ -> false

let p_general_address_literal p state =
  (Logs.debug @@ fun m -> m "state: p_general_address_literal");

  p_ldh_str (fun tag state ->
             Lexer.p_chr ':' state;
             let content = Lexer.p_while is_dcontent state in
             (* XXX:  we already try to parse IPv6 tag, so if we are in this
                      case, we have an invalid IPv6 data.
                TODO: mey be it's useful to associate a lexer with the tag
                      and try this. I have no time for that.
             *)
             if Iana.Set.exists ((=) tag) Iana.tag && tag <> "IPv6"
             then p (tag, content) state
             else if tag = "IPv6"
             then raise (Lexer.Error (Lexer.err_invalid_ipv6 state))
             else raise (Lexer.Error (Lexer.err_invalid_tag tag state)))
    state

let p_address_literal p =
  Lexer.p_try_rule
    (fun ipv4 -> p (`IPv4 ipv4))
    (Lexer.p_try_rule
      (fun ipv6 -> p (`IPv6 ipv6))
      (fun state ->
        p_general_address_literal
          (fun (tag, content) -> p (`General (tag, content)))
          state)
      (p_ipv6_address_literal (fun v6 state -> `Ok (v6, state))))
    (p_ipv4_address_literal (fun v4 state -> `Ok (v4, state)))
