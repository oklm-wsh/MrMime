let is_tspecials = function
  | '(' | ')' | '<' | '>'  | '@'
  | ',' | ';' | ':' | '\\' | '"'
  | '/' | '[' | ']' | '?'  | '=' -> true
  | _ -> false

let is_token chr =
  not (is_tspecials chr)
  && not (Rfc822.is_ctl chr)
  && not (Rfc822.is_space chr)

let p_token state = Lexer.p_while is_token state

let p_attribute = p_token

let p_ietf_token p state =
  let token = p_token state in
  p (`Ietf_token token) state

let p_iana_token p state =
  let token = p_token state in
  p (`Iana_token token) state

let p_x_token p state =
  Lexer.p_set ['X'; 'x'] state;
  Lexer.p_chr '-' state;

  p (`X_token (p_token state)) state

let p_extension_token p state =
  match Lexer.cur_chr state with
  | 'X' | 'x' -> p_x_token p state
  | chr       -> p_ietf_token p state

let p_composite_type p state =
  match String.lowercase @@ p_token state with
  | "message"   -> p `Message state
  | "multipart" -> p `Multipart state
  | extension_token ->
    p_extension_token (fun t _ -> p t state) (Lexer.of_string extension_token)

let p_discrete_type p state =
  match String.lowercase @@ p_token state with
  | "text" -> p `Text state
  | "image" -> p `Image state
  | "audio" -> p `Audio state
  | "video" -> p `Video state
  | "application" -> p `Application state
  | extension_token ->
    p_extension_token (fun t _ -> p t state) (Lexer.of_string extension_token)

let p_msg_id = Rfc5322.p_msg_id

let p_mechanism p state =
  match String.lowercase @@ p_token state with
  | "7bit" -> p `Bit7 state
  | "8bit" -> p `Bit8 state
  | "binary" -> p `Binary state
  | "quoted-printable" -> p `Quoted_printable state
  | "base64" -> p `Base64 state
  | extension_token ->
    let state' = Lexer.of_string extension_token in
    match Lexer.cur_chr state with
    | 'X' | 'x' -> p_x_token (fun t _ -> p t state) state'
    | chr       -> p_ietf_token (fun t _ -> p t state) state'

let p_type p state =
  match String.lowercase @@ p_token state with
  (* discrete-type *)
  | "text" -> p `Text state
  | "image" -> p `Image state
  | "audio" -> p `Audio state
  | "video" -> p `Video state
  | "application" -> p `Application state
  (* composite-type *)
  | "message"   -> p `Message state
  | "multipart" -> p `Multipart state
  (* extension-type *)
  | extension_token ->
    p_extension_token (fun t _ -> p t state) (Lexer.of_string extension_token)

let p_subtype p state =
  match Lexer.cur_chr state with
  | 'X' | 'x' -> p_extension_token p state
  | chr       -> p_ietf_token p state (* | p_iana_token p state *)

let p_value p =
  Lexer.p_try_rule
    (fun data -> p (`String data))
    (fun state -> p (`Token (p_token state)) state)
    (Rfc5322.p_quoted_string (fun data state -> `Ok (data, state)))

let p_parameter p state =
  let name = p_attribute state in

  Lexer.p_chr '=' state;
  p_value (fun value -> p (name, value)) state
