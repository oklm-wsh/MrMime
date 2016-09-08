let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

module Decoder =
struct
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

  let from_input input = Lexing.from_function
    (fun lexbuf n ->
     Input.transmit input @@ fun buff off len ->
       let m = min n len in
       Internal_buffer.blit_bytes buff off lexbuf 0 m; m)

  let ensure_line =
    let rec loop i s fail succ =
      let has_crlf = ref false in

      let _ = Input.transmit i (fun buff off len ->
        let has_cr = ref false in
        let i      = ref 0 in

        while !i < len && not (!has_cr && Internal_buffer.get buff (off + !i) = '\n')
        do if Internal_buffer.get buff (off + !i) = '\r'
           then has_cr := true
           else has_cr := false;

           incr i
        done;

        if !i < len && !has_cr && Internal_buffer.get buff (off + !i) = '\n'
        then has_crlf := true;

        0)
      in

      if !has_crlf
      then succ i s ()
      else if s = Complete then fail i s [] IO.End_of_flow
      else let succ' i' s' = loop i' s' fail succ in
           let fail' i' s' = fail i' s' [] IO.End_of_flow in
           IO.prompt i succ' fail'
    in

    { f = loop }

  let fast_qp buffer =
    ensure_line
    *> { f = fun i s fail succ ->
         let mark = Input.mark i in

         let k_eof buffer lexbuf = fail i s [] IO.End_of_flow in
         let k_done line_breaks buffer lexbuf =
           Input.unmark mark i;
           Input.radvance i (lexbuf.Lexing.lex_curr_pos - 2);
           (* TODO: need to be check! *)

           succ i s line_breaks in

         let lexbuf = from_input i in

         Fast_qp.decode buffer k_done k_eof lexbuf }

  let line buffer boundary =
    let is_ = function
      | '=' | ' ' | '\r' | '\t' -> false
      | _ -> true
    in
    (fast_qp buffer <* { f = fun i s fail succ ->
                         succ i s () } >>= function
     | `Hard -> (boundary *> return (`End true))
                <|> (Rfc822.crlf
                     *> { f = fun i s fail succ -> Buffer.add_char buffer '\n';
                                                   succ i s `Newline })
     | `Soft -> (boundary *> return (`End true))
                <|> (Rfc822.crlf *> return `Newline))
    <|> ({ f = fun i s fail succ ->
           let n = Input.transmit i @@ fun buff off len ->

             let len' = locate buff off len is_ in
             Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
             len'
           in

           succ i s n } *> peek_chr >>= function
         | None -> return (`End false)
         | Some '=' ->
            (hex >>= fun chr ->
             { f = fun i s fail succ ->
              Buffer.add_char buffer chr; succ i s `Hex })
           <|> (char '=' *> ((boundary *> return (`End true))
                             <|> (Rfc822.crlf *> return `Newline)))
         | Some ' '
         | Some '\t' ->
           repeat (Some 1) None Rfc822.is_wsp
           >>= fun lwsp -> peek_chr >>= (function
            | Some '\r' -> return `Whitespace
            | _ ->
              { f = fun i s fail succ ->
                Buffer.add_string buffer lwsp;

                succ i s `Whitespace })
         | Some '\r' ->
           (boundary *> return (`End true))
           <|> (Rfc822.crlf *> { f = fun i s fail succ ->
                                 Buffer.add_char buffer '\n';
                                 succ i s `Newline })
         | Some chr -> return (`Dirty chr))

  let decode boundary rollback buffer =
    (fix @@ fun m -> line buffer boundary >>= function
     | `End r -> return (r, Buffer.contents buffer)
     | _ -> m)
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
end

module Encoder =
struct
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
end

module Input = Parser.Input

type 'a decoder =
  { src           : 'a Input.t
  ; buffer        : Buffer.t
  ; mutable i     : Bytes.t
  ; mutable i_off : int
  ; mutable i_len : int
  ; mutable k     : 'a decoder -> decoding }
and decoding =
  [ `Continue
  | `Error  of Parser.err
  | `Dirty  of char
  | `End    of string
  | `String of string ]

let pp_decoding fmt = function
  | `Continue   -> Format.fprintf fmt "`Continue"
  | `Error exn  -> Format.fprintf fmt "`Error exn"
  | `Dirty chr  -> Format.fprintf fmt "`Dirty %S" (String.make 1 chr)
  | `End str    -> Format.fprintf fmt "`End %s" str
  | `String str -> Format.fprintf fmt "`String %s" str

let string' { buffer; _ } =
  let s = Buffer.contents buffer in
  Buffer.clear buffer; `String s

let terminate rollback { buffer; _ } =
  let s = Buffer.contents buffer in
  Buffer.clear buffer; `End (rollback, s)

let boundary' boundary t =
  let open Parser in
  boundary *> return (terminate true t)

let hard t =
  let open Parser in
  Rfc822.crlf
  *> { f = fun i s fail succ ->
       Buffer.add_char t.buffer '\n';
       succ i s () }
  *> return (string' t)

let soft t =
  let open Parser in
  Rfc822.crlf
  *> return (string' t)

let p boundary t =
  let open Parser in
  let open Parser.Convenience in
  let is_ = function
    | '=' | ' ' | '\r' | '\t' -> false
    | _ -> true
  in
  (Decoder.fast_qp t.buffer
   >>= function
       | `Hard -> ((boundary' boundary t) <|> (hard t))
       | `Soft -> ((boundary' boundary t) <|> (soft t)))
  <|> ({ f = fun i s fail succ ->
         let n = Input.transmit i @@ fun buff off len ->

           let len' = locate buff off len is_ in
           Buffer.add_string t.buffer (Internal_buffer.sub_string buff off len');
           len'
         in

         succ i s n } *> peek_chr >>= function
       | None -> return (terminate false t)
       | Some '=' ->
         (Decoder.hex >>= fun chr ->
          Buffer.add_char t.buffer chr; return (string' t))
         <|> (char '=' *> ((boundary *> return (terminate true t))
                           <|> (Rfc822.crlf *> return (string' t))))
       | Some ' '
       | Some '\t' ->
         repeat (Some 1) None Rfc822.is_wsp
         >>= fun lwsp -> peek_chr >>= (function
          | Some '\r' -> return (string' t)
          | _ ->
            { f = fun i s fail succ ->
              Buffer.add_string t.buffer lwsp;

              succ i s () } *> return (string' t))
       | Some '\r' ->
         (boundary *> return (terminate true t))
         <|> (Rfc822.crlf *> { f = fun i s fail succ -> Buffer.add_char t.buffer '\n';
                                                        succ i s () }
                          *> return (string' t))
       | Some chr -> return (`Dirty chr))

let rec loop ((boundary, rollback) as p') t = function
  | Parser.Read { buffer; k; } ->
    t.k <- (fun t ->
            Input.write_string
              t.src
              (Bytes.unsafe_to_string t.i)
              t.i_off t.i_len;

            let s = if t.i_len = 0
                    then Parser.Complete
                    else Parser.Incomplete
            in

            loop p' t @@ k t.i_len s);
    `Continue
  | Parser.Fail (marks, exn) -> `Error exn
  | Parser.Done (`Dirty chr) ->
    t.k <- (fun t -> loop p' t
            @@ Parser.(run t.src (p boundary t)));
    `Dirty chr
  | Parser.Done (`String raw) ->
    t.k <- (fun t -> loop p' t
            @@ Parser.(run t.src (p boundary t)));
    `String raw
  | Parser.Done (`End (false, raw)) ->
    t.k <- (fun t -> loop p' t
            @@ Parser.(run t.src (return (`End (false, raw)))));
    `End raw
  | Parser.Done (`End (true, raw)) ->
    match Parser.run t.src rollback with
    | Parser.Done () ->
      t.k <- (fun t -> loop p' t
              @@ Parser.(run t.src (return (`End (false, raw)))));
      `End raw
    | _ -> assert false

let decoder_src t = t.src

let decoder ((boundary, rollback) as p') src =
  { src
  ; buffer = Buffer.create 16
  ; i      = Bytes.empty
  ; i_off  = 0
  ; i_len  = 0
  ; k      = fun t -> loop p' t
                      @@ Parser.(run t.src (Convenience.option
                                            (`End (false, ""))
                                            (Rfc822.crlf *> p boundary t))) }

let decode t = t.k t

let src t buf off len =
  if (off < 0 || len < 0 || off + len > Bytes.length buf)
  then raise (Invalid_argument "QuotedPrintable.src");

  t.i <- buf;
  t.i_off <- off;
  t.i_len <- len;
