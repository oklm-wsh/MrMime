let is_bcharsnospace = function
  | '\'' | '(' | ')' | '+' | '_' | ','
  | '-' | '.' | '/' | ':' | '=' | '?' -> true
  | chr -> Rfc822.is_alpha chr || Rfc822.is_digit chr

let is_bchars = function
  | ' ' -> true
  | chr -> is_bcharsnospace chr

let is_valid_bchars str =
  let i = ref 0 in

  while !i < String.length str
        && is_bchars (String.get str !i)
  do incr i done;

  !i = String.length str

let p_boundary p state =
  let b = Lexer.p_repeat ~a:0 ~b:69 is_bcharsnospace state in
  p b state

let p_dash_boundary p state =
  Lexer.p_str "--" state;
  p_boundary p state

let p_dash_boundary' boundary p state =
  Lexer.p_str "--" state;
  Lexer.p_str boundary state;
  p state

let m_dash_boundary boundary =
  "--" ^ boundary

let p_transport_padding p state =
  let _ = Lexer.p_repeat Rfc822.is_lwsp state in
  p state

let p_delimiter p state =
  Rfc822.p_crlf @@ p_dash_boundary p

(* See RFC 2046 § 5.1.1:

   discard-text := *( *text CRLF) *text
                   ; May be ignored or discarded.

   and RFC 822 § 3.3:

   text        =  <any CHAR, including bare    ; => atoms, specials,
                   CR & bare LF, but NOT       ;  comments and
                   including CRLF>             ;  quoted-strings are
                                               ;  NOT recognized.
*)
let p_discard_text stop p state =
  let rec text has_text state =
    let rec aux = function
      | `Stop state -> p has_text state
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ Lexer.safe k i))
      | #Lexer.err as err ->  err
      | `Continue state ->
        match Lexer.cur_chr state with
        | chr -> Lexer.junk_chr state; text true state
    in aux @@ Lexer.safe (stop has_text) state
  in

  text false state

(* See RFC 2046 § 5.1.1:

   preamble := discard-text
   epilogue := discard-text

   XXX: with and without check of [dash-boundary] in [preamble]
*)
let p_preamble = p_discard_text
let p_epilogue = p_discard_text

(* See RFC 2046 § 5.1.1:

   delimiter := CRLF dash-boundary

   XXX: need to be compose with [dash-boundary]
*)
let p_delimiter' boundary p state =
  Rfc822.p_crlf (p_dash_boundary' boundary p) state

let m_delimiter boundary =
  "\r\n" ^ (m_dash_boundary boundary)

let p_close_delimiter' boundary p state =
  p_delimiter' boundary (fun state -> Lexer.p_str "--" state; p state) state

let m_close_delimiter boundary =
  (m_delimiter boundary) ^ "--"

let p_body_part' boundary p_octet p state =
  let next fields =
    Lexer.p_try_rule
      (fun data -> p (Some data))
      (p None)
      (Rfc822.p_crlf @@ p_octet fields)
  in

  Lexer.p_try_rule next
    (next [])
    (Rfc2045.p_mime_part_headers'
     (fun field -> raise (Lexer.Error (Lexer.err_invalid_field field state)))
     (fun fields state -> `Ok (fields, state)))
    state

(* See RFC 2046 § 5.1.1:

   encapsulation := delimiter transport-padding

   XXX: it's not the same as RFC 2046. we need to stop at the begin of the body
        because it may be encoded (with Base64 or QuotedPrintable) and it's
        ineffective to save the date and recompute in Base64/QuotedPrintable.
*)
let p_encapsulation' boundary p_octet p =
  p_delimiter' boundary
    (p_transport_padding @@ Rfc822.p_crlf @@ p_body_part' boundary p_octet p)

(* See RFC 2046 § 5.1.1:

   multipart-body := [preamble CRLF]
                     dash-boundary transport-padding CRLF
                     body-part

   XXX: same as [p_encapsulation']
*)
let p_multipart_body' boundary p_octet p =
  let stop has_text =
    let dash_boundary = m_dash_boundary boundary in
    Lexer.p_try_rule
      (fun () -> Lexer.roll_back (fun state -> `Stop state) ((if has_text then "\r\n" else "") ^ dash_boundary))
      (fun state -> `Continue state)
      (match has_text with
       | false -> p_dash_boundary' boundary (fun state-> `Ok ((), state))
       | true  -> Rfc822.p_crlf @@ p_dash_boundary' boundary (fun state-> `Ok ((), state)))
  in
  let stop_at_crlfcrlf state =
    Lexer.p_try_rule
      (fun () -> Lexer.roll_back (fun state -> `Stop state) "\r\n\r\n")
      (fun state -> `Continue state)
      (Rfc822.p_crlf @@ Rfc822.p_crlf @@ (fun state -> `Ok ((), state)))
  in
  let rec next acc =
    Lexer.p_try_rule
      (fun data -> next (data :: acc))
      (p_close_delimiter' boundary @@ p_transport_padding
       @@ Lexer.p_try_rule
            (fun () -> p (List.rev acc))
            (p (List.rev acc))
            (Rfc822.p_crlf @@ p_epilogue stop_at_crlfcrlf (fun _ state -> `Ok ((), state))))
      (p_encapsulation' boundary p_octet (fun data state -> `Ok (data, state)))
  in

  p_preamble stop
    (function
     | false ->
       p_dash_boundary' boundary
       @@ p_transport_padding
       @@ Rfc822.p_crlf
       @@ p_body_part' boundary p_octet
       @@ fun data -> next [data]
     | true ->
       Rfc822.p_crlf
       @@ p_dash_boundary' boundary
       @@ p_transport_padding
       @@ Rfc822.p_crlf
       @@ p_body_part' boundary p_octet
       @@ fun data -> next [data])
