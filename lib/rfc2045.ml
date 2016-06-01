open BaseDecoder

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

type field =
  [ `ContentType of content
  | `ContentEncoding of encoding
  | `ContentID of id
  | `ContentDescription of string
  | `Content of string * Rfc5322.phrase
  | `Unsafe of string * Rfc5322.phrase ]

type mime_field =
  [ `MimeVersion of int * int ]

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

let p_token p state = p_while is_token p state

let p_attribute p state = p_token (fun token -> p (String.lowercase_ascii token)) state

let p_ietf_token p =
  p_token
  @ fun token -> p (`Ietf_token token)

let p_iana_token p =
  p_token
  @ fun token -> p (`Iana_token token)

let p_x_token p =
  p_set ['X'; 'x']
  @ p_chr '-'
  @ p_token
  @ fun token -> p (`X_token token)

let p_extension_token p =
  cur_chr @ function
  | 'X' | 'x' -> p_x_token p
  | chr       -> p_ietf_token p

let p_composite_type p =
  p_token @ function
  | "message"   -> p `Message
  | "multipart" -> p `Multipart
  | extension_token ->
    fun state ->
      p_extension_token
        (fun t _ -> p t state)
        (Decoder.of_string extension_token)

let p_discrete_type p =
  p_token @ function
  | "text"  -> p `Text
  | "image" -> p `Image
  | "audio" -> p `Audio
  | "video" -> p `Video
  | "application" -> p `Application
  | extension_token ->
    fun state ->
      p_extension_token
        (fun t _ -> p t state)
        (Decoder.of_string extension_token)

let p_msg_id = Rfc822.p_msg_id

let p_type p =
  p_token @ fun token -> match String.lowercase_ascii token with
  (* discrete-type *)
  | "text" -> p `Text
  | "image" -> p `Image
  | "audio" -> p `Audio
  | "video" -> p `Video
  | "application" -> p `Application
  (* composite-type *)
  | "message"   -> p `Message
  | "multipart" -> p `Multipart
  (* extension-type *)
  | extension_token ->
    fun state ->
      (p_extension_token
       @ fun t _ -> p t state)
      (Decoder.of_string extension_token)

let ty_to_string = function
  | `Text -> "text"
  | `Image -> "image"
  | `Audio -> "audio"
  | `Video -> "video"
  | `Application -> "application"
  | `Message -> "message"
  | `Multipart -> "multipart"
  | `X_token s | `Ietf_token s -> s

let p_subtype ty p =
  cur_chr @ function
  | 'X' | 'x' -> p_extension_token p
  | chr       ->
    p_token
    @ fun token ->
      try Iana.Map.find ty Iana.mtype
          |> Iana.Set.find token
          |> fun value -> p (`Iana_token value)
      with exn -> p (`X_token token)

let p_value p =
  (Rfc822.p_quoted_string (fun data state -> `Ok (data, state)))
  / (p_token @ fun token -> p (`Token token))
  @ (fun data -> p (`String data))

let p_parameter p =
  p_attribute
  @ fun name -> p_chr '='
  @ p_value
  @ fun value -> p (name, value)

let p_content p =
  let rec loop p =
    let rec aux acc =
      cur_chr @ function
      | ';' ->
        junk_chr
        @ Rfc822.p_fws
        @ fun _ _ -> p_parameter
        @ fun parameter -> Rfc822.p_fws
        @ fun _ _ -> aux (parameter :: acc)
      | chr -> p (List.rev acc)
    in

    aux []
  in
  Rfc822.p_cfws
  @ fun _ -> p_type
  @ fun ty -> p_chr '/'
  @ Rfc822.p_cfws
  @ fun _ -> p_subtype (ty_to_string ty)
  @ fun subty -> Rfc822.p_cfws
  @ fun _ -> cur_chr
  @ function
    | ';' -> loop @ fun parameters -> Rfc822.p_cfws @ fun _ -> p (ty, subty, parameters)
    | chr -> p (ty, subty, [])

let p_version p =
  Rfc822.p_cfws
  @ fun _ -> p_while Rfc822.is_digit
  @ fun a -> let a = int_of_string a in Rfc822.p_cfws
  @ fun _ -> p_chr '.'
  @ Rfc822.p_cfws
  @ fun _ -> p_while Rfc822.is_digit
  @ fun b -> let b = int_of_string b in Rfc822.p_cfws
  @ fun _ -> p (a, b)

let p_mechanism p =
  p_token @ fun token -> match String.lowercase_ascii token with
  | "7bit" -> p `Bit7
  | "8bit" -> p `Bit8
  | "binary" -> p `Binary
  | "quoted-printable" -> p `QuotedPrintable
  | "base64" -> p `Base64
  | extension_token ->
    fun state ->
      p_extension_token
        (fun t _ -> p t state)
        (Decoder.of_string extension_token)

let p_encoding p =
  Rfc822.p_cfws
  @ fun _ -> p_mechanism
  @ fun e -> Rfc822.p_cfws
  @ fun _ -> p e

let p_id p =
  Rfc822.p_cfws
  @ fun _ -> Rfc822.p_msg_id
  @ fun m -> Rfc822.p_cfws
  @ fun _ -> p m

let p_field mime_extend extend field p =
  [%debug Printf.printf "state: p_field (RFC 2045) %s\n%!" field];

  let field = String.lowercase_ascii field in

  let rule =
    match field with
    | "content-type" -> p_content @ fun c -> Rfc822.p_crlf @ ok (`ContentType c)
    | "content-transfer-encoding" -> p_encoding @ fun e -> Rfc822.p_crlf @ ok (`ContentEncoding e)
    | "content-id" -> p_id @ fun i -> Rfc822.p_crlf @ p (`ContentID i)
    | "content-description" -> Rfc822.p_text @ fun _ s -> Rfc822.p_crlf @ ok (`ContentDescription s)
    | field ->
      (* XXX: the optionnal-field [fields] is handle by RFC 822 or RFC 5322.
              in this case, we raise an error. *)
      if String.length field >= 8 && String.sub field 0 8 = "content-"
      then let field = String.sub field 8 (String.length field - 8) in
           p_try_rule p
             (Rfc5322.p_unstructured @ fun value ->
              Rfc822.p_crlf @ p (`Content (field, value)))
             (mime_extend field @ ok)
      else extend field p
  in

  rule
  / (Rfc5322.p_unstructured
     @ fun l -> Rfc822.p_crlf
     @ p (`Unsafe (field, l)))
  @ p

let p_entity_headers extend_mime extend p state =
  [%debug Printf.printf "state: p_entity_header\n%!"];

  let rec loop acc =
    (Rfc822.p_field_name
     @ fun field -> (0 * 0) Rfc822.is_lwsp
     @ fun _ -> p_chr ':'
     @ p_field extend_mime extend field
     @ fun data state -> `Ok (data, state))
    / (p (List.rev acc))
    @ (fun field -> loop (field :: acc))
  in

  loop [] state

let p_mime_message_headers extend_mime extend field p =
  [%debug Printf.printf "state: p_mime_message_header\n%!"];

  match field with
  | "mime-version" -> p_version @ fun v -> Rfc822.p_crlf @ p (`MimeVersion v)
  | field -> p_field extend_mime extend field p

let p_mime_part_headers = p_entity_headers

module Base64 = Base64
module QuotedPrintable = QuotedPrintable
