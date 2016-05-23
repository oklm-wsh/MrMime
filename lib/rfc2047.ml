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

let p_encoding_text p state =
  p_while (function ' ' | '?' -> false | chr -> true) p state

let p_token p state =
  let is chr =
    if is_especials chr
       || Rfc822.is_ctl chr
       || Rfc822.is_space chr
    then false
    else true
  in

  p_while is p state

let p_charset  = p_token
let p_encoding = p_token

let p_encoded_word p =
  p_str "=?"
  @ p_charset
  @ fun charset -> p_chr '?'
  @ p_encoding
  @ fun encoding -> p_chr '?'
  @ match String.uppercase encoding with
    | "Q" ->
      QuotedPrintable.p_inline_decode
        ((p_str "?=" @ fun state -> `Ok ((), state))
         / (fun state -> `Continue state)
         @ (fun () state -> `Stop state))
        (p charset QuotedPrintable)
    | "B" ->
      Base64.p_decode
        ((p_str "?=" @ fun state -> `Ok ((), state))
         / (fun state -> `Continue state)
         @ (fun () state -> `Stop state))
        (p charset Base64)
    | enc -> fun state -> raise (Error.Error (Error.err_unexpected_encoding enc state))

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
  (p_encoded_word
   @ fun charset encoding data state -> `Ok ((charset, encoding, data), state))
  / (rule p)
  @ (fun encoded -> p (`Encoded encoded))
