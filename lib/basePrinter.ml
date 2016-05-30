let p = Format.fprintf

let pp_list ?(sep = "") pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> p fmt "%a%s" pp_data x sep; aux r
  in

  aux lst

let pp_string ?(in_qs = false) ?(in_dm = false) fmt =
  String.iter
    (function
     | '\x07' -> p fmt "\\a"
     | '\b'   -> p fmt "\\b"
     | '\t'   -> p fmt "\\t"
     | '\n'   -> p fmt "\\n"
     | '\x0b' -> p fmt "\\v"
     | '\r'   -> p fmt "\\r"
     | '\\'   -> p fmt "\\\\"
     | ' ' when in_qs -> p fmt " "
     | '"' when in_qs -> p fmt "\\\""
     | ']' when in_dm -> p fmt "\\]"
     | chr when Rfc5322.is_vchar chr -> p fmt "%c" chr
     | chr    -> p fmt "\\%c" chr)

let pp_atom fmt = function
  | `Atom s -> pp_string fmt s

let pp_word fmt = function
  | `Atom s -> pp_string fmt s
  | `String s -> p fmt "\"%a\"" (pp_string ~in_qs:true ~in_dm:false) s
  | `Encoded (charset, encoding, s) -> pp_string fmt s

let pp_phrase =
  let pp_encoded charset encoding fmt data =
    Rfc2047.p_decoded_word
      charset encoding
      (fun data _ -> `Ok data)
      (Lexer.of_string data)
    |> function
       | `Ok data -> p fmt "%s" data
       | _        -> assert false
  in
  let pp_elt fmt = function
    | `WSP -> p fmt " "
    | `FWS -> p fmt "\r\n\t"
    | `CR n -> p fmt "%s" (String.make n '\r')
    | `LF n -> p fmt "%s" (String.make n '\n')
    | `Dot -> p fmt "."
    | #Rfc5322.word as elt -> pp_word fmt elt
    | `Encoded (charset, encoding, data) ->
      p fmt "%a" (pp_encoded charset encoding) data
  in
  let rec loop fmt = function
    | [] -> ()
    | [ x ] -> pp_elt fmt x
    | (`Encoded _ as a) :: (`Encoded _ as b) :: r ->
        p fmt "%a %a" pp_elt a pp_elt b; loop fmt r
    | (`Encoded _ as a) :: (`Atom _ as b) :: r
    | (`Atom _ as a) :: (`Encoded _ as b) :: r ->
        p fmt "%a %a" pp_elt a pp_elt b; loop fmt r
    | x :: r -> pp_elt fmt x; loop fmt r
  in
  loop
