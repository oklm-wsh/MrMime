open BaseLexer

type field =
  [ Header.field
  | Content.field
  | MimeVersion.field ]

(* composition between RFC 5322 and RFC 2045 about the header *)
let p_header p state =
  [%debug Printf.printf "state: p_header\n%!"];

  Rfc5322.p_header
    (Rfc2045.p_mime_message_headers
      (fun field p state -> raise (Error.Error (Error.err_nothing_to_do state)))
      (fun field p state -> raise (Error.Error (Error.err_nothing_to_do state))))
    p state

(* check of the header, eg. {!val:Header.of_lexer} and {!val:Content.of_lexer} *)
let c_header fields p state =
  Header.of_lexer fields
    (fun header rest state ->
     match header with
     | Some header ->
       Content.of_lexer rest
         (fun content rest state ->
          p header content state) state
     | None -> raise (Error.Error (Error.err_invalid_header state)))
    state

(* composition betweeen RFC 5322/body, RFC 2045/Base64/body and RFC
   2045/QuotedPrintable/body with the [boundary] *)
let p_body boundary content =
  let stop =
    match boundary with
    | Some boundary ->
      let delimiter = Rfc2046.m_delimiter boundary in
      let close_delimiter = Rfc2046.m_close_delimiter boundary in
      p_try_rule
        (fun () -> roll_back (fun state -> `Stop state) close_delimiter)
        (p_try_rule
           (fun () -> roll_back (fun state -> `Stop state) delimiter)
           (fun state -> `Continue state)
           (Rfc2046.p_delimiter boundary (fun state -> `Ok ((), state))))
        (Rfc2046.p_close_delimiter boundary (fun state -> `Ok ((), state)))
    | None ->
      (* TODO: may be it's wrong, I don't see anything about the end of message
               and I don't know if we stop really at [CRLF CRLF]. *)
      p_try_rule
        (fun () state -> `Stop state)
        (fun state -> `Continue state)
        (to_end_of_file
         (fun state -> match peek_chr state with
                       | None -> `Ok ((), state)
                       | Some chr -> raise (Error.Error (Error.err_unexpected chr state))))
  in

  match Content.encoding content with
  | `Ietf_token _
  | `X_token _
  | `Binary | `Bit8 | `Bit7 -> Rfc5322.p_body stop
  | `Base64                 -> Rfc2045.Base64.p_decode stop
  | `QuotedPrintable        -> Rfc2045.QuotedPrintable.p_decode stop

let field_of_lexer = function
  | #Rfc5322.field as x -> (Header.field_of_lexer x :> field)
  | #Rfc2045.field as x -> (Content.field_of_lexer x :> field)
  | #Rfc2045.mime_field as x -> (MimeVersion.field_of_lexer x :> field)

(* compute the multipart explained in RFC 2046 § 5 *)
let rec p_multipart boundary p_body' p state =
  [%debug Printf.printf "state: p_multipart\n%!"];

  Rfc2046.p_multipart_body boundary None
    (fun fields next ->
       let rec aux p_body' boundary fields next =
         [%debug Printf.printf "state: p_multipart/aux\n%!"];

         Content.of_lexer fields @@ fun content rest ->
           match ContentType.ty @@ Content.ty content with
           | #Rfc2045.other
           | #Rfc2045.discrete ->
             [%debug Printf.printf "state: p_multipart/aux (discrete)\n%!"];

             p_body' content (fun data -> next (`Discrete ((content, List.map field_of_lexer rest), data)))
           | #Rfc2045.composite ->
             [%debug Printf.printf "state: p_multipart/aux (composite)\n%!"];

             let parameters = ContentType.parameters @@ Content.ty content in
             try let boundary' = Rfc2045.value_to_string @@ List.assoc "boundary" parameters in
                 Rfc2046.p_multipart_body
                   boundary' (Some boundary)
                   (aux (p_body (Some boundary')) boundary')
                   (fun data -> next (`Composite ((content, List.map field_of_lexer rest), data)))
             with Not_found -> raise (Error.Error (Error.err_expected_boundary state))
       in aux p_body' boundary fields next)
    p state

(* top-level of the multipart compute *)
let rec switch content p_body' p_discrete p_composite state =
  [%debug Printf.printf "state: switch\n%!"];

  match ContentType.ty @@ Content.ty content with
  (* unknow case, so TODO! *)
  | #Rfc2045.other
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
          (p_body (Some boundary))
          (fun data -> p_composite data)
          state
    with Not_found -> raise (Error.Error (Error.err_expected_boundary state))

(* the fucking message ... *)
let p_message p =
  [%debug Printf.printf "state: p_message\n%!"];

  p_header
    (fun fields ->
       c_header fields
         (fun header content ->
            (switch content (p_body None)
                (fun data -> p header (`Discrete (content, data)))
                (fun data -> p header (`Composite (content, data))))))
