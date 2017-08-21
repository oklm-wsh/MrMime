type word   = [ `Atom of string | `String of string ]
type domain = [ `Domain of string list | `Literal of string ]
type local  = word list
type msg_id = (local * domain)

open Parser
open Parser.Convenience

let is_vchar = function
  | '\x21' .. '\x7e' -> true
  | _ -> false

let is_obs_no_ws_ctl = function
  | '\001' .. '\008'
  | '\011'
  | '\012'
  | '\014' .. '\031'
  | '\127' -> true
  | _ -> false

let is_ctext = function
  | '\033' .. '\039'
  | '\042' .. '\091'
  | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let is_qtext = function
  | '\033'
  | '\035' .. '\091'
  | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let is_atext = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&'
  | '\''
  | '*' | '+' | '-' | '/' | '='
  | '?' | '^' | '_' | '`' | '{'
  | '}' | '|' | '~' -> true
  | _ -> false

let is_cr = (=) '\r'
let is_lf = (=) '\n'
let is_d0 = (=) '\000'

let is_wsp = function
  | '\x09' | '\x20' -> true
  | _ -> false

let is_quoted_pair chr =
  is_vchar chr
  || is_wsp chr
  || is_d0 chr
  || is_obs_no_ws_ctl chr
  || is_lf chr
  || is_cr chr

let is_dtext = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let of_escaped_character = function
  | '\x61' -> '\x07'
  | '\x62' -> '\x08'
  | '\x74' -> '\x09'
  | '\x6E' -> '\x0A'
  | '\x76' -> '\x0B'
  | '\x66' -> '\x0C'
  | '\x72' -> '\x0D'
  | c      -> c

let quoted_pair =
  char '\\'
  *> satisfy is_quoted_pair
  >>= fun x -> return (of_escaped_character x)

let wsp = satisfy (function '\x09' | '\x20' -> true | _ -> false)

let crlf = char '\r' *> char '\n' *> return ()

let obs_fws =
  let many' p = fix
    @@ fun m -> (lift2 (fun _ _ -> (true, true)) p m)
                <|> return (false, false)
  in
  one wsp
  *> (many' (crlf *> (one wsp)))
  >>| fun (has_crlf, has_wsp) -> (true, has_crlf, has_wsp)

let fws =
  let many' p = fix
    @@ fun m -> (lift2 (fun _ _ -> (true, true)) p m)
                <|> return (false, true)
  in
  obs_fws
  <|> ((option (false, false) (many' wsp <* crlf))
       >>= fun (has_wsp, has_crlf) -> one wsp
       *> return (has_wsp, has_crlf, true))

let ignore p = p *> return ()

let comment =
  (fix @@ fun comment ->
   let ccontent = (ignore @@ Rfc6532.str is_ctext)
                   <|> (ignore quoted_pair)
                   <|> (ignore comment)
   in
   char '('
   *> (many ((option (false, false, false) fws) *> ccontent))
   *> (option (false, false, false) fws)
   *> char ')')
  *> return ()

let cfws =
  ((one ((option (false, false, false) fws)
         *> comment)
    *> (option (false, false, false) fws))
   <|> fws)
  *> return ()

let cfws = cfws <?> "cfws"

let qcontent =
  (Rfc6532.str is_qtext)
  <|> (quoted_pair >>| String.make 1)

(* TODO: optimize with Buffer module. *)
let quoted_string =
  (option () cfws)
  *> char '"'
  *> (many (option (false, false, false) fws
            >>= fun (has_wsp, _, has_wsp') -> qcontent
            >>= fun s -> return (if has_wsp || has_wsp'
                                 then (String.concat "" [" "; s])
                                 else s))
      >>= fun pre -> option (false, false, false) fws
      >>= fun (has_wsp, _, has_wsp') -> return (if has_wsp || has_wsp'
                                                then " " :: pre
                                                else pre))
  <* char '"'
  >>| String.concat ""
  <* (option () cfws)

let atom =
  (option () cfws)
  *> (Rfc6532.str is_atext)
  <* (option () cfws)

let word =
  (atom >>| fun s -> `Atom s)
  <|> (quoted_string >>| fun s -> `String s)

let obs_local_part =
  let p = word in
  let s = char '.' in
  fix (fun m -> lift2 (fun x r -> x :: r) p ((s *> m) <|> return []))

let dot_atom_text =
  let p = Rfc6532.str is_atext in
  let s = char '.' in
  fix (fun m -> lift2 (fun x r -> x :: r) p ((s *> m) <|> return []))

let dot_atom =
  (option () cfws) *> dot_atom_text <* (option () cfws)

let local_part =
  obs_local_part
  <|> (dot_atom >>| List.map (fun x -> `Atom x))
  <|> (quoted_string >>| fun s -> [`String s])

let obs_domain =
  atom
  >>= fun x -> fix (fun m -> (lift2 (fun x r -> x :: r) (char '.' *> atom) m)
                             <|> return [])
  >>| fun r -> x :: r

let domain_literal =
  (option () cfws)
  *> char '['
  *> (many ((option (false, false, false) fws)
            *> ((Rfc6532.str is_dtext) <|> (quoted_pair >>| String.make 1)))
      >>| String.concat "")
  <* (option (false, false, false) fws)
  <* char ']'
  <* (option () cfws)

let domain =
  (obs_domain >>| fun domain -> `Domain domain)
  <|> (domain_literal >>| fun literal -> `Literal literal)
  <|> (dot_atom >>| fun domain -> `Domain domain)

let id_left = local_part <|> (dot_atom_text >>| List.map (fun x -> `Atom x))

let no_fold_literal =
  char '['
  *> Rfc6532.dtext is_dtext
  <* char ']'

let id_right =
  (no_fold_literal >>| fun literal -> `Literal literal)
  <|> domain
  <|> (dot_atom_text >>| fun domain -> `Domain domain)

let msg_id =
  (option () cfws)
  *> char '<'
  *> id_left
  >>= fun left -> char '@'
  *> id_right
  >>= fun right -> char '>'
  *> (option () cfws)
  >>| fun () -> (left, right)
