let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

open Parser
open Parser.Convenience

let is_hex = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let hex a b =
  let aux code = match code with
    | '0' .. '9' -> (Char.code code) - (Char.code '0') + 0
    | 'a' .. 'f' -> (Char.code code) - (Char.code 'a') + 10
    | 'A' .. 'F' -> (Char.code code) - (Char.code 'A') + 10
    | _ -> assert false
  in Char.chr (((aux a) * 16) + (aux b))

let hex =
  char '='
  *> satisfy is_hex
  >>= fun a -> satisfy is_hex
  >>= fun b -> return (hex a b)

let decode boundary rollback buffer =
  let is_ = function
    | '=' | '\r' | '\x09' | '\x20' -> false
    | _ -> true
  in
  (fix @@ fun m ->
   { f = fun i s fail succ ->
     let n = Input.transmit i @@ fun buff off len ->
       let len' = locate buff off len is_ in
       Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
       len'
     in

     succ i s n } *> peek_chr >>= function
   | None -> return (false, Buffer.contents buffer)
   | Some '=' ->
     (hex >>= fun chr ->
      Buffer.add_char buffer chr; m)
     <|> (char '=' *> ((boundary *> return (true, Buffer.contents buffer))
                       <|> (Rfc822.crlf *> m)))
   | Some '\r' ->
     (boundary *> return (true, Buffer.contents buffer))
     <|> (Rfc822.crlf >>= fun () -> Buffer.add_char buffer '\n'; m)
   | Some ('\x20' | '\x09') ->
     repeat (Some 1) None Rfc822.is_wsp
     >>= fun lwsp ->
       (boundary *> return (true, Buffer.contents buffer))
       <|> (Rfc822.crlf >>= fun () -> Buffer.add_char buffer '\n';
                                      m)
       <|> ({ f = fun i s fail succ ->
              Buffer.add_string buffer lwsp;

              succ i s () } *> m)
   | Some chr -> m)
  >>= function
    | true, content  ->
      rollback *> return content
    | false, content ->
      return content

let inline buffer =
  fix @@ fun m ->
    peek_chr >>= function
    | None
    | Some '?' -> return (Buffer.contents buffer)
    | Some '=' -> hex >>= fun chr -> Buffer.add_char buffer chr; m
    | Some '_' -> advance 1 >>= fun () -> Buffer.add_char buffer ' '; m
    | Some chr -> advance 1 >>= fun () -> Buffer.add_char buffer chr; m

let inline () =
  inline (Buffer.create 16)

let decode boundary rollback =
  decode boundary rollback (Buffer.create 16)

module T =
struct
  open Encoder

  let _to = "0123456789ABCDEF"

  type t =
    { state            : Encoder.t
    ; word             : Buffer.t
    ; mutable position : int }

  let lift k state =
    k { state
      ; word     = Buffer.create 16
      ; position = 0 }

  let add_break k ({ state; position; _ } as t) =
    if position > 0
    then string "=\r\n" (fun state -> k { t with position = 0; state = state }) state
    else k t

  let commit_word k ({ position; word; } as t) =
    (if position + Buffer.length word >= 76
     then add_break
     else noop)
    (fun ({ state; word; position; } as t) ->
     string
       (Buffer.contents word)
       (fun state ->
        let len = Buffer.length word in
        Buffer.clear word; k { t with state = state; position = position + len; })
       state)
    t

  let wrap_bigword length k ({ word; _ } as t) =
    (if Buffer.length word >= 76 - length
     then add_break $ commit_word $ add_break
     else noop) k t

  let add_char chr =
    wrap_bigword 1
    $ fun k ({ word; _ } as t) ->
      Buffer.add_char word chr;
      k t

  let add_quoted_char chr k =
    wrap_bigword 3
      (fun ({ word; _ } as t) ->

       let code = Char.code chr in
       let h    = (code lsr 4) land (16 - 1) in
       let l    =  code        land (16 - 1) in

       Buffer.add_char word '=';
       Buffer.add_char word _to.[h];
       Buffer.add_char word _to.[l];

       k t)

  let add_wsp = function
    | `Space ->
      add_char '\x20' $ commit_word
    | `Tab ->
      add_char '\x09' $ commit_word

  let add_newline chr =
    let rec aux k ({ state; _ } as t) =
      string "\r\n" (fun state -> k { t with state = state; position = 0 }) state
    in
    (match chr with
     | Some chr -> add_quoted_char chr
     | None -> noop)
    $ commit_word $ aux

  let flush k t =
    commit_word k t

  let unlift k t =
    flush (fun { state; _ } -> k state) t
end

let is_safe_char = function
  | '\033' .. '\060'
  | '\062' .. '\126' -> true
  | _                -> false

let explode str =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (str.[i] :: l) in
  exp (String.length str - 1) []

let w_inline_encode str =
  let open Encoder in
  List.fold_right
    (function
     | '\x20' -> char '_'
     | '\x09' -> string "=09"
     | '?'    -> string "=3F"
     | '_'    -> string "=5F"
     | '='    -> string "=3D"
     | chr    ->
       if is_safe_char chr
       then char chr
       else let code = Char.code chr in
            let h    = (code lsr 4) land (16 - 1) in
            let l    = code land (16 - 1) in
            char '=' $ char (String.get T._to h) $ char (String.get T._to l))
    (explode str)

let w_encode content k =
  let len = String.length content in

  let recognize_newline idx =
    if idx < len && idx + (String.length "\n") <= len
    then let n = ref 0 in
         let r = ref true in
         while idx + !n < len
               && !n < String.length "\n"
         do if String.get "\n" !n <> String.get content (idx + !n)
            then r := false;

            incr n;
         done;

         !r
    else false
  in

  let rec loop idx k =
    if idx < len
    then match String.get content idx with
         | '\x20' ->
           if recognize_newline (idx + 1)
           then T.add_newline (Some '\x20') @@ loop (idx + 2) k (* TODO: +2 only if the newline is LF (+3 if it's CRLF on windows) *)
           else T.add_wsp `Space @@ loop (idx + 1) k
         | '\x09' ->
           if recognize_newline (idx + 1)
           then T.add_newline (Some '\x09') @@ loop (idx + 2) k
           else T.add_wsp `Tab @@ loop (idx + 1) k
         (* Unix newline encoder *)
         | '\n' when true -> (* is LF *)
           T.add_newline None @@ loop (idx + 1) k
         (* Windows newline encoder *)
         | '\r' when false && (idx + 1 < len) && String.get content (idx + 1) = '\n' -> (* is CRLF *)
           T.add_newline None @@ loop (idx + 2) k
         | chr when is_safe_char chr ->
           T.add_char chr @@ loop (idx + 1) k
         | chr ->
           T.add_quoted_char chr @@ loop (idx + 1) k
    else T.flush k
  in

  T.lift (loop 0 @@ T.unlift k)
