type raw =
  | QuotedPrintable of string
  | Base64 of Base64.result

open Parser
open Parser.Convenience

type err += Invalid_charset

let is_especials = function
  | '(' | ')'
  | '<' | '>'
  | '@' | ','
  | ';' | ':'
  | '"' | '/'
  | '[' | ']'
  | '?' | '.'
  | '=' -> true
  | _ -> false

let is_ctl = function
  | '\000' .. '\031' -> true
  | _ -> false

let is_space = (=) ' '

let token =
  recognize (fun chr -> not (is_especials chr || is_ctl chr || is_space chr))
  >>= fun token ->
    { f = fun i s fail succ ->

      if String.length token > 0
      then succ i s token
      else fail i s [] Invalid_charset }

external id : 'a -> 'a = "%identity"

let inline_encoded_string =
  string id "=?"
  *> token
  >>= fun charset -> char '?'
  *> satisfy (function 'Q' | 'B' -> true | _ -> false)
  >>= (function
    | 'Q' -> return `Q
    | 'B' -> return `B
    | _   -> assert false)
  >>= fun encoding -> char '?'
  >>= fun _ -> (match encoding with
    | `B -> Base64.inline () >>| fun v -> Base64 v
    | `Q -> QuotedPrintable.inline () >>| fun v -> QuotedPrintable v)
  >>= fun decoded -> string id "?="
  *> return (charset, decoded)
