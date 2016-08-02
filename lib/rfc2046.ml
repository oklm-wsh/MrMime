let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

open Parser
open Parser.Convenience

type ('field, 'a) octet = (MrMime_content.t -> ([ Rfc5322.field | Rfc2045.field | Rfc5322.skip ] as 'field) list -> 'a t)

let is_bcharsnospace = function
  | '\'' | '(' | ')' | '+' | '_' | ','
  | '-' | '.' | '/' | ':' | '=' | '?' -> true
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | '0' .. '9' -> true
  | _ -> false

let is_bchars = function
  | ' ' -> true
  | c -> is_bcharsnospace c

let make_dash_boundary boundary =
  "--" ^ boundary

let make_delimiter boundary =
  "\r\n" ^ (make_dash_boundary boundary)

let make_close_delimiter boundary =
  (make_delimiter boundary) ^ "--"

(* XXX: you can rollback just after. *)
let dash_boundary boundary =
  let string s = string (fun x -> x) s in
  string ("--" ^ boundary) *> return ()

let discard_to_dash_boundary boundary =
  (fix @@ fun m ->
   { f = fun i s fail succ ->
     let discard buff off len = locate buff off len ((<>) '-') in

     let n = Input.transmit i discard in
     succ i s n } *> peek_chr >>= function
   | Some '-' -> (dash_boundary boundary
                  *> return true) <|> (advance 1 *> m)
   | Some chr -> m
   | None -> return false)
  >>= fun has_boundary ->
     { f = fun i s fail succ ->

       if has_boundary
       then Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) @@ "--" ^ boundary);
       succ i s () }

let transport_padding =
  repeat None None (function '\x09' | '\x20' -> true | _ -> false)
  *> return ()

let text =
  fix @@ fun m ->
    { f = fun i s fail succ ->
      let discard buff off len = locate buff off len ((=) '\r') in

      let n = Input.transmit i discard in
      succ i s n } *> peek_chr >>= function
    | None -> return ()
    | Some '\r' ->
      (advance 1 *> peek_chr >>= function
       | None -> return ()
       | Some '\n' ->
         { f = fun i s fail succ ->
           Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) "\r");

           succ i s () }
       | _ -> m)
    | _ -> m

let discard_text =
  let many p = fix (fun m -> (p *> m) <|> return ()) in
  many (many text *> Rfc822.crlf) *> many text

(* XXX: you can rollback just after. *)
let delimiter boundary =
  let string s = string (fun x -> x) s in
  string ("\r\n--" ^ boundary) *> return ()

(* XXX: you can rollback just after. *)
let close_delimiter boundary =
  delimiter (boundary ^ "--") *> return ()

let discard_to_delimiter boundary =
  (fix @@ fun m ->
   { f = fun i s fail succ ->
     let discard buff off len = locate buff off len ((<>) '\r') in
     let n = Input.transmit i discard in
     succ i s n } *> peek_chr >>= function
   | Some '\r' -> (delimiter boundary
                   *> return true) <|> (advance 1 *> m)
   | Some chr -> m
   | None -> return false)
  >>= fun has_boundary ->
     let f
       : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k
       = fun i s fail succ ->

         if has_boundary
         then Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) ("\r\n--" ^ boundary));
         succ i s ()
     in { f }

let body_part octet =
  Rfc2045.mime_part_headers
    (Rfc5322.field (fun _ -> fail Rfc5322.Nothing_to_do))
  >>= MrMime_content.Decoder.part >>= fun (content, fields) ->
    ((Rfc822.crlf *> return `HasCRLF) <|> (return `NoCRLF))
  >>= (function
    | `HasCRLF -> octet content fields >>| fun v -> Some v
    | `NoCRLF -> return None)
  >>| fun octets -> (content, fields, octets)

let encapsulation boundary octet =
  delimiter boundary
  *> transport_padding
  *> Rfc822.crlf
  *> body_part octet

let preamble boundary = discard_to_dash_boundary boundary
let epilogue parent = match parent with
  | Some boundary -> discard_to_delimiter boundary
  | None ->
    fix @@ fun m ->
    { f = fun i s fail succ ->
      let _ = Input.transmit i (fun _ _ len -> len) in
      succ i s () } *> peek_chr >>= function
    | None -> return ()
    | Some _ -> m

let optimized_encapsulation boundary octet =
  let fix' f =
    let rec u a = lazy (f r a)
    and r a = { f = fun i s fail succ -> Lazy.(force (u a)).f i s fail succ } in
    r
  in

  fix' @@ fun m l ->
    ((delimiter boundary *> transport_padding *> Rfc822.crlf *> return `HasBoundary)
     <|> (return `NoBoundary))
    >>= function
      | `HasBoundary -> body_part octet >>= fun x -> m (x :: l)
      | `NoBoundary -> return l

let multipart_body parent boundary octet =
  option () (preamble boundary)
  *> dash_boundary boundary
  *> transport_padding
  *> Rfc822.crlf
  *> body_part octet
  >>= fun x -> optimized_encapsulation boundary octet []
  >>= fun r ->
    (close_delimiter boundary
     *> transport_padding
     *> option () (Rfc822.crlf *> (epilogue parent))
     *> return (x :: r)) <|> (return (x :: r))
