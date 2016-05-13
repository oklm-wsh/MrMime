open BaseLexer

type encoding =
  | QuotedPrintable
  | Base64

type encoded =
  [ `Encoded of string * encoding * string ]

let is_especials = function
  | '(' | ')'
  | '<' | '>'
  | '@' | ','
  | ';' | ':'
  | '"' | '/'
  | '[' | ']'
  | '?' | '.'
  | '=' -> true
  | chr -> false

let p_encoding_text =
  p_while
    (function ' ' | '?' -> false | chr -> true)

let p_token =
  let is chr =
    if is_especials chr
       || Rfc822.is_ctl chr
       || Rfc822.is_space chr
    then false
    else true
  in

  p_while is

let p_charset  = p_token
let p_encoding = p_token

let p_encoded_word p state =
  (Logs.debug @@ fun m -> m "state: p_encoded_word");

  p_str "=?" state;
  let charset = p_charset state in
  p_chr '?' state;
  let encoding = p_encoding state in
  p_chr '?' state;

  (Logs.debug @@ fun m -> m "state: p_encoded_word (charset: %s)" charset);

  match String.uppercase encoding with
  | "Q" ->
    QuotedPrintable.p_inline_decode
      (p_try_rule
         (fun () state -> `Stop state)
         (fun state -> `Continue state)
         (fun state -> ignore @@ p_str "?=" state; `Ok ((), state)))
      (p charset QuotedPrintable) state
  | "B" ->
    Base64.p_decode
      (p_try_rule
         (fun () state -> `Stop state)
         (fun state -> `Continue state)
         (fun state -> ignore @@ p_str "?=" state; `Ok ((), state)))
      (p charset Base64) state
  | enc -> raise (Error.Error (Error.err_unexpected_encoding enc state))

let p_decoded_word charset encoding p state =
  let buf = Buffer.create 16 in

  Buffer.add_string buf "=?";
  Buffer.add_string buf charset;
  Buffer.add_string buf "?";
  Buffer.add_string buf
    (match encoding with QuotedPrintable -> "Q" | Base64 -> "B");
  Buffer.add_string buf "?";

  match encoding with
  | QuotedPrintable ->
    QuotedPrintable.p_inline_encode
      (fun state ->
       if state.Lexer.pos = state.Lexer.len
       then `Stop state
       else `Continue state)
      (fun encoded state ->
       Buffer.add_string buf encoded;
       Buffer.add_string buf "?=";
       p (Buffer.contents buf) state)
      state
  | Base64 ->
    Base64.p_inline_encode
      (fun state ->
       if state.Lexer.pos = state.Lexer.len
       then `Stop state
       else `Continue state)
      (fun encoded state ->
       Buffer.add_string buf encoded;
       Buffer.add_string buf "?=";
       p (Buffer.contents buf) state)
      state

let p_try_rule p rule =
  (Logs.debug @@ fun m -> m "state: p_try (RFC 2047)");

  p_try_rule
    (fun encoded -> p (`Encoded encoded))
    (rule p)
    (p_encoded_word (fun charset encoding data state ->
                     `Ok ((charset, encoding, data), state)))
