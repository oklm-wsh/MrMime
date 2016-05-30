open BaseLexer

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

let p_dash_boundary boundary p =
  p_str "--"
  @ p_str boundary
  @ p

let m_dash_boundary boundary =
  "--" ^ boundary

let p_transport_padding p =
  [%debug Printf.printf "state: p_transport_padding\n%!"];

  (0 * 0) Rfc822.is_lwsp
  @ fun _ state -> [%debug Printf.printf "state: p_transport_padding end\n%!"]; p state

(* See RFC 2046 § 5.1.1:

   discard-text := *( *text CRLF) *text
                   ; May be ignored or discarded.

   and RFC 822 § 3.3:

   text        =  <any CHAR, including bare    ; => atoms, specials,
                   CR & bare LF, but NOT       ;  comments and
                   including CRLF>             ;  quoted-strings are
                                               ;  NOT recognized.
*)
let p_discard_text stop p =
  let rec text has_text state =
    let rec aux = function
      | `Stop state -> p has_text state
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err ->  err
      | `Continue state ->
        (cur_chr @ function
         | chr -> junk_chr @ text true)
        state
    in aux @@ safe (stop has_text) state
  in

  text false

(* See RFC 2046 § 5.1.1:

   preamble := discard-text
   epilogue := discard-text
*)
let p_preamble p state = p_discard_text p state
let p_epilogue p state = p_discard_text p state

(* See RFC 2046 § 5.1.1:

   delimiter := CRLF dash-boundary

   XXX: need to be compose with [dash-boundary]
*)
let p_delimiter boundary p =
  Rfc822.p_crlf @ p_dash_boundary boundary p

let m_delimiter boundary =
  "\r\n" ^ (m_dash_boundary boundary)

let p_close_delimiter boundary p state =
  [%debug Printf.printf "state: p_close_delimiter %s\n%!" boundary];

  (p_delimiter boundary
   @ p_str "--"
   @ (fun state -> [%debug "state: p_close_delimiter match\n%!"]; p state))
  state

let m_close_delimiter boundary =
  (m_delimiter boundary) ^ "--"

(* See RFC 2046 § 5.1:

   body-part := MIME-part-headers [CRLF *OCTET]
                ; Lines in a body-part must not start
                ; with the specified dash-boundary and
                ; the delimiter must not appear anywhere
                ; in the body part.  Note that the
                ; semantics of a body-part differ from
                ; the semantics of a message, as
                ; described in the text.

   XXX: [p_octet] must be stop to the boundary
*)
let p_body_part (type data) boundary p_octet p =
  [%debug Printf.printf "state: p_body_part\n%!"];

  let next fields =
    (Rfc822.p_crlf
     @ p_octet fields
     @ fun data state -> `Ok ((data : data), state))
    / (p None)
    @ (fun data -> p (Some (data : data)))
  in

  (Rfc2045.p_mime_part_headers
     (fun field next state -> raise (Error.Error (Error.err_invalid_field field state)))
     (Rfc5322.p_field @ fun field _ state -> raise (Error.Error (Error.err_invalid_field field state)))
     (fun fields state -> `Ok (fields, state)))
  / (next [])
  @ next

(* See RFC 2046 § 5.1.1:

   encapsulation := delimiter transport-padding
                    CRLF body-part
*)
let p_encapsulation boundary p_octet p state =
  [%debug Printf.printf "state: p_encapsulation\n%!"];

  (p_delimiter boundary
   @ p_transport_padding @ Rfc822.p_crlf @ p_body_part boundary p_octet p)
  state

(* See RFC 2046 § 5.1.1:

   multipart-body := [preamble CRLF]
                     dash-boundary transport-padding CRLF
                     body-part *encapsulation
                     close-delimiter
                     transport-padding
                     [CRLF epilogue]
*)
let p_multipart_body boundary parent_boundary p_octet p =
  [%debug Printf.printf "state: p_multipart [boundary: %s and parent boundary: %s]\n%!"
   boundary
   (match parent_boundary with Some x -> x | None -> "<none>")];

  let stop_preamble has_text =
    let dash_boundary = m_dash_boundary boundary in
    p_try_rule
      (fun () state ->
       roll_back
         (fun state -> `Stop state)
         dash_boundary
         state)
      (fun state -> `Continue state)
      (p_dash_boundary boundary (fun state-> `Ok ((), state)))
  in
  let stop_epilogue state =
    [%debug Printf.printf "state: p_multipart (stop epilogue)\n%!"];

    match parent_boundary with
    | None ->
      [%debug Printf.printf "state: p_multipart (stop epilogue) to end\n%!"];

      to_end_of_file
      ((fun state -> match peek_chr state with
       | None -> `Ok ((), state)
       | Some chr -> raise (Error.Error (Error.err_unexpected chr state)))
       / (fun state -> `Continue state)
       @ (fun () state -> `Stop state))
    | Some boundary ->
      let delimiter = m_delimiter boundary in
      let close_delimiter = m_close_delimiter boundary in
      p_try_rule
        (fun () -> roll_back (fun state -> `Stop state) close_delimiter)
        (p_try_rule
           (fun () -> roll_back (fun state -> `Stop state) delimiter)
           (fun state -> `Continue state)
           (p_delimiter boundary (fun state -> `Ok ((), state))))
        (p_close_delimiter boundary (fun state -> [%debug Printf.printf "state: p_multipart (stop epilogue) close delimiter\n%!"]; `Ok ((), state)))
  in
  let rec next acc =
    [%debug Printf.printf "state: p_multipart/next\n%!"];

    (p_encapsulation boundary p_octet @ fun data state -> `Ok (data, state))
    / (p_close_delimiter boundary
       @ p_transport_padding
       @ (to_end_of_file ((Rfc822.u_crlf
           @ p_epilogue stop_epilogue
           @ fun _ state -> `Ok ((), state)))
          / (p (List.rev acc))
          @ fun () -> p (List.rev acc)))
    @ fun data -> next (data :: acc)
  in

  p_preamble stop_preamble
  @ fun has_preamble ->
    p_dash_boundary boundary
    @ p_transport_padding
    @ Rfc822.p_crlf
    @ p_body_part boundary p_octet
    @ fun data -> next [data]
