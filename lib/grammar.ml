open Base

let p_header p state =
  Rfc5322.p_header
    (Rfc2045.p_mime_message_headers
     (fun field p state -> raise (Lexer.Error (Lexer.err_nothing_to_do state))))
    p state

let c_header fields p state =
  Header.of_lexer fields
    (fun header rest state ->
     match header with
     | Some header ->
       Content.of_lexer rest
         (fun content rest state ->
          match rest with
          | [] -> p header content state
          | _ -> raise (Lexer.Error (Lexer.err_invalid_header state))) state
     | None -> raise (Lexer.Error (Lexer.err_invalid_header state)))
    state

let p_body boundary content p state =
  let stop =
    match boundary with
    | Some boundary ->
      let delimiter = Rfc2046.m_delimiter boundary in
      let close_delimiter = Rfc2046.m_close_delimiter boundary in
      Lexer.p_try_rule
        (fun () -> Lexer.roll_back (fun state -> `Stop state) delimiter)
        (Lexer.p_try_rule
           (fun () -> Lexer.roll_back (fun state -> `Stop state) close_delimiter)
           (fun state -> `Continue state)
           (Rfc2046.p_delimiter' boundary (fun state -> `Ok ((), state))))
        (Rfc2046.p_close_delimiter' boundary (fun state -> `Ok ((), state)))
    | None ->
      (* TODO: may be it's wrong, I don't see anything about the end of message
               and I don't know if we stop really at [CRLF CRLF]. *)
      Lexer.p_try_rule
        (fun () -> Lexer.roll_back (fun state -> `Stop state) "\r\n\r\n")
        (fun state -> `Continue state)
        (Rfc822.p_crlf @@ Rfc822.p_crlf (fun state -> `Ok ((), state)))
  in

  match Content.encoding content with
  | `Binary | `Bit8 | `Bit7 -> Rfc5322.p_body stop p state
  | `Base64                 -> Rfc2045.Base64.p_decode stop p state
  | `QuotedPrintable        -> Rfc2045.Base64.p_decode stop p state
  (* unknow case, so TODO! *)
  | `Ietf_token _
  | `X_token _ -> raise (Lexer.Error (Lexer.err_nothing_to_do state))

let p_multipart boundary content self p_body' p state =
  Rfc2046.p_multipart_body' boundary
    (fun fields ->
     Content.of_lexer fields
     (fun content rest state ->
      match rest with
      | [] ->
        self content p_body'
          (fun data -> fun state -> `Ok (`Discrete data, state))
          (fun data -> fun state -> `Ok (`Composite data, state))
          state
      | _  -> raise (Lexer.Error (Lexer.err_invalid_header state))))
    p
  state

let rec switch content p_body' p_discrete p_composite state =
  match ContentType.ty @@ Content.ty content with
  (* unknow case, so TODO! *)
  | #Rfc2045.other     -> raise (Lexer.Error (Lexer.err_nothing_to_do state))
  (* discrete top-level media types *)
  | #Rfc2045.discrete  -> p_body' content (fun data -> p_discrete data) state
  (* composite top-level media types *)
  | #Rfc2045.composite ->
    (* See RFC 2046 § 5.1.1:

       The  Content-Type field  for multipart  entities requires  one parameter,
       "boundary".  The  boundary  delimiter line  is  then  defined  as  a line
       consisting  entirely of  two hyphen  characters ("-",  decimal  value 45)
       followed by  the boundary  parameter value  from the  Content-Type header
       field, optional linear whitespace, and a terminating CRLF.
    *)
    let parameters = ContentType.parameters @@ Content.ty content in
    try let boundary = (Rfc2045.value_to_string @@ List.assoc "boundary" parameters) in
        p_multipart
          boundary
          content
          switch
          (p_body (Some boundary))
          (fun data -> p_composite data)
          state
    with Not_found -> raise (Lexer.Error (Lexer.err_nothing_to_do state))

let p_message p state =
  p_header
  (fun fields ->
   c_header fields
   (fun header content ->
    switch content (p_body None) (fun data -> p header (`Discrete data)) (fun data -> p header (`Composite data))))
