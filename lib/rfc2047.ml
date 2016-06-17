open BaseDecoder

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

(* TODO: handle utf-8? *)
let p_token p state =
  let is_ctl = function
    | '\000' .. '\031' -> true
    | _ -> false
  in
  let is_space = (=) '\x20' in
  let is chr = not (is_especials chr || is_ctl chr || is_space chr) in

  p_while is p state

let p_charset  = p_token
let p_encoding = p_token

let p_encoded_word p =
  p_str "=?"
  @ p_charset
  @ fun charset -> p_chr '?'
  @ p_encoding
  @ fun encoding -> p_chr '?'
  @ match String.uppercase_ascii encoding with
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

let p_try_rule p rule =
  (p_encoded_word
   @ fun charset encoding data state -> `Ok ((charset, encoding, data), state))
  / (rule p)
  @ (fun encoded -> p (`Encoded encoded))

open BaseEncoder

let w_decoded_word charset encoding content =
  w "=?"
  $ w charset
  $ w "?"
  $ (match encoding with
     | QuotedPrintable -> w "Q"
     | Base64          -> w "B")
  $ w "?"
  $ (match encoding with
     | QuotedPrintable -> QuotedPrintable.w_inline_encode content
     | Base64 -> Base64.w_inline_encode content)
  $ w "?="
