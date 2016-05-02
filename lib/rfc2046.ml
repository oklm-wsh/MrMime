let is_bcharsnospace = function
  | '\'' | '(' | ')' | '+' | '_' | ','
  | '-' | '.' | '/' | ':' | '=' | '?' -> true
  | chr -> Rfc822.is_alpha chr || Rfc822.is_digit chr

let is_bchars = function
  | ' ' -> true
  | chr -> is_bcharsnospace chr

let p_boundary p state =
  let b = Lexer.p_repeat ~a:0 ~b:69 is_bcharsnospace state in
  p b state

let p_dash_boundary p state =
  Lexer.p_str "--" state;
  p_boundary p state

let p_transport_padding p state =
  let _ = Lexer.p_repeat Rfc822.is_lwsp state in
  p state

let p_delimiter p state =
  Rfc822.p_crlf @@ p_dash_boundary p

let p_discard p state =
  let rec loop _ =
    Rfc822.p_text
    @@ fun _ -> Rfc822.p_crlf
    @@ fun state ->
        Lexer.p_try_rule
          (fun () -> Lexer.roll_back p "\r\n")
          (loop true)
          (Rfc822.p_crlf (fun state -> `Ok ((), state)))
          state
  in
  Rfc822.p_text loop state

let p_preamble = p_discard
let p_epilogue = p_discard
