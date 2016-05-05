type discrete =
  [ `Application
  | `Audio
  | `Image
  | `Text
  | `Video ]
type composite =
  [ `Message
  | `Multipart ]
type other =
  [ `Ietf_token of string
  | `X_token of string ]
type ty = [ discrete | composite | other ]

type subty =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token of string ]

type value =
  [ `String of string
  | `Token of string ]

type content = ty * subty * (string * value) list
type version = int * int
type encoding =
  [ `Base64
  | `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Ietf_token of string
  | `X_token of string ]
type id = Rfc822.msg_id

let value_to_string = function
  | `String s -> s
  | `Token s  -> s

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
  | "text"  -> p `Text state
  | "image" -> p `Image state
  | "audio" -> p `Audio state
  | "video" -> p `Video state
  | "application" -> p `Application state
  | extension_token ->
    p_extension_token (fun t _ -> p t state) (Lexer.of_string extension_token)

let p_msg_id = Rfc822.p_msg_id

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

let ty_to_string = function
  | `Text -> "text"
  | `Image -> "image"
  | `Audio -> "audio"
  | `Video -> "video"
  | `Application -> "application"
  | `Message -> "message"
  | `Multipart -> "multipart"
  | `X_token s | `Ietf_token s -> s

let p_subtype ty p state =
  match Lexer.cur_chr state with
  | 'X' | 'x' -> p_extension_token p state
  | chr       ->
    let token = p_token state in
    try Iana.Map.find ty Iana.mtype
        |> Iana.Set.find token
        |> fun value -> p (`Iana_token value) state
    with exn -> p (`X_token token) state

let p_value p =
  Lexer.p_try_rule
    (fun data -> p (`String data))
    (fun state -> p (`Token (p_token state)) state)
    (Rfc822.p_quoted_string (fun data state -> `Ok (data, state)))

let p_parameter p state =
  (Logs.debug @@ fun m -> m "state: p_parameter");
  let name = p_attribute state in

  Lexer.p_chr '=' state;
  p_value (fun value -> p (name, value)) state

let p_content p state =
  (Logs.debug @@ fun m -> m "state: p_content");

  let rec loop p state =
    let rec aux acc state =
      (Logs.debug @@ fun m -> m "state: p_content/loop");

      match Lexer.cur_chr state with
      | ';' ->
        Lexer.junk_chr state;
        Rfc822.p_fws
          (fun _ _ ->
           p_parameter
             (fun parameter ->
              Rfc822.p_fws (fun _ _ -> aux (parameter :: acc))))
          state
      | chr -> p (List.rev acc) state
    in

    aux [] state
  in
  Rfc822.p_cfws
    (fun _ -> p_type (fun ty state ->
      Lexer.p_chr '/' state;
      Rfc822.p_cfws (fun _ ->
        p_subtype (ty_to_string ty) (fun subty ->
        Rfc822.p_cfws (fun _ state ->
          match Lexer.cur_chr state with
          | ';' -> loop (fun parameters -> Rfc822.p_cfws (fun _ -> p (ty, subty, parameters))) state
          | chr -> p (ty, subty, []) state)))
    state))
  state

let p_version p state =
  Rfc822.p_cfws (fun _ state ->
    let a = int_of_string @@ Lexer.p_while Rfc822.is_digit state in
    Rfc822.p_cfws (fun _ state ->
      Lexer.p_chr '.' state;
      Rfc822.p_cfws (fun _ state ->
        let b = int_of_string @@ Lexer.p_while Rfc822.is_digit state in
        Rfc822.p_cfws (fun _ -> (Logs.debug @@ fun m -> m "state: p_version (%d.%d)" a b); p (a, b)) state)
      state)
    state)
  state

let p_mechanism p state =
  match String.lowercase @@ p_token state with
  | "7bit" -> p `Bit7 state
  | "8bit" -> p `Bit8 state
  | "binary" -> p `Binary state
  | "quoted-printable" -> p `QuotedPrintable state
  | "base64" -> p `Base64 state
  | extension_token ->
    p_extension_token (fun t _ -> p t state) (Lexer.of_string extension_token)

let p_encoding p =
  Rfc822.p_cfws (fun _ -> p_mechanism (fun e -> Rfc822.p_cfws (fun _ -> p e)))

let p_id p =
  Rfc822.p_cfws (fun _ -> Rfc822.p_msg_id (fun m -> Rfc822.p_cfws (fun _ -> p m)))

let p_entity_headers extend field p state =
  let rule =
    match field with
    | "content-type" -> p_content (fun c -> Rfc822.p_crlf @@ p (`ContentType c))
    | "content-encoding" -> p_encoding (fun e -> Rfc822.p_crlf @@ p (`ContentEncoding e))
    | "content-id" -> p_id (fun i -> Rfc822.p_crlf @@ p (`ContentID i))
    | "content-description" -> Rfc822.p_text (fun s -> Rfc822.p_crlf @@ p (`ContentDescription s))
    | field ->
      (* XXX: the optionnal-field [fields] is handle by RFC 822 or RFC 5322.
              in this case, we raise an error. *)
      if String.sub field 0 8 = "content-"
      then let field = String.sub field 8 (String.length field - 8) in
           Lexer.p_try_rule p
             (Rfc822.p_text @@ (fun value -> Rfc822.p_crlf @@ p (`Content (field, value))))
             (extend field (fun data state -> `Ok (data, state)))
      else raise (Lexer.Error (Lexer.err_invalid_field field state))
  in

  rule state

let p_entity_headers' extend p state =
  let rec loop acc state =
    Lexer.p_try_rule
      (fun field -> loop (field :: acc))
      (p (List.rev acc))
      (fun state ->
        let field = Rfc822.p_field_name state in
        let _     = Lexer.p_repeat Rfc822.is_lwsp state in

        Lexer.p_chr ':' state;

        p_entity_headers extend field (fun data state -> `Ok (data, state)) state)
      state
  in

  loop [] state

let p_mime_message_headers extend field p =
  match field with
  | "mime-version" -> p_version (fun v -> Rfc822.p_crlf @@ p (`MimeVersion v))
  | field -> p_entity_headers extend field p

let p_mime_part_headers = p_entity_headers
let p_mime_part_headers' = p_entity_headers'

module Base64 = Base64
module QuotedPrintable = QuotedPrintable
