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

let p_encoding_text = Lexer.p_while (function ' ' | '?' -> false | chr -> true)

let p_token =
  let is chr =
    if is_especials chr
       || Rfc822.is_ctl chr
       || Rfc822.is_space chr
    then false
    else true
  in

  Lexer.p_while is

let p_charset  = p_token
let p_encoding = p_token

let p_encoded_word p state =
  Lexer.p_str "=?" state;
  let charset = p_charset state in
  Lexer.p_chr '?' state;
  let encoding = p_encoding state in
  Lexer.p_chr '?' state;

  match String.uppercase encoding with
  | "Q" ->
    QuotedPrintable.p_decode
      (Lexer.p_try_rule
         (fun () state -> `Stop state)
         (fun state -> `Continue state)
         (fun state -> ignore @@ Lexer.p_str "?=" state; `Ok ((), state)))
      (p charset) state
  | "B" ->
    Base64.p_decode
      (Lexer.p_try_rule
         (fun () state -> `Stop state)
         (fun state -> `Continue state)
         (fun state -> ignore @@ Lexer.p_str "?=" state; `Ok ((), state)))
      (p charset) state
  | enc -> raise (Lexer.Error (Lexer.err_unexpected_encoding enc state))

let p_try p =
  (Logs.debug @@ fun m -> m "state: p_try (RFC 2047)");

  Lexer.p_try_rule
    (fun really_data -> Lexer.roll_back p really_data) p
    (p_encoded_word (fun _ data state -> `Ok (data, state)))
