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

let pp_phrase fmt =
  let pp_elt fmt = function
    | `Dot -> p fmt "."
    | `FWS -> p fmt " "
    | #Rfc5322.word as elt -> pp_word fmt elt
  in
  pp_list pp_elt fmt
