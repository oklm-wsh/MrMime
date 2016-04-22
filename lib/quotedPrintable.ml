module F =
struct
  let add_newline buf =
    Buffer.add_char buf '\n'

  let hex a b =
    let aux code = match code with
      | '0'..'9' -> (Char.code code) - (Char.code '0') + 0
      | 'A'..'F' -> (Char.code code) - (Char.code 'A') + 10
      | 'a'..'f' -> (Char.code code) - (Char.code 'a') + 10
      | _ -> raise (Invalid_argument "QuotedPrintable.F.hex")
    in Char.chr (((aux a) * 16) + (aux b))

  let hex s =
    let aux code = match code with
      | '0'..'9' -> (Char.code code) - (Char.code '0') + 0
      | 'A'..'F' -> (Char.code code) - (Char.code 'A') + 10
      | 'a'..'f' -> (Char.code code) - (Char.code 'a') + 10
      | _ -> raise (Invalid_argument "QuotedPrintable.F.hex")
    in Char.chr ((aux (String.get s 0) * 16) + (aux (String.get s 0)))

  let add_char buf chr =
    Buffer.add_char buf chr

  let add_string buf str =
    Buffer.add_string buf str
end

module T =
struct
  let _to = "0123456789ABCDEF"

  type t =
    {
      word             : Buffer.t;
      mutable position : int;
    }

  let make () =
    { word = Buffer.create 16
    ; position = 0 }

  let add_break buf ({ position; _ } as t) =
    if position > 0
    then begin
      Buffer.add_string buf "=\n";
      t.position <- 0;
    end

  let commit_word buf ({ position; word; } as t) =
    if position + Buffer.length word >= 76
    then add_break buf t;

    Buffer.add_buffer buf word;
    t.position <- t.position + Buffer.length word;
    Buffer.clear word

  (* XXX: or TODO, does not handle a word with size > 76 *)
  let wrap_bigword buf ({ word; _ } as t) length =
    if Buffer.length word >= 76 then add_break buf t;

    Buffer.add_buffer buf word;
    t.position <- t.position + Buffer.length word;
    Buffer.clear word

  let add_char buf ({ word; _ } as t) chr =
    wrap_bigword buf t 1;
    Buffer.add_char word chr

  let add_quoted_char buf ({ word; _ } as t) chr =
    wrap_bigword buf t 3;

    let code = Char.code chr in
    let h    = (code lsr 4) land (16 - 1) in
    let l    =  code        land (16 - 1) in

    Buffer.add_char word '=';
    Buffer.add_char word _to.[h];
    Buffer.add_char word _to.[l]

  let add_wsp buf t chr =
    add_char buf t chr;
    commit_word buf t

  let add_newline buf t chr =
    (match chr with
     | Some chr -> add_quoted_char buf t chr
     | None -> ());
    commit_word buf t;
    Buffer.add_char buf '\n';
    t.position <- 0

  let flush buf t =
    commit_word buf t
end

(* See RFC 2045 § 6.7:

   safe-char       := <any octet with decimal value of 33 through
                       60 inclusive, and 62 through 126>
                       ; Characters not listed as "mail-safe" in
                       ; RFC 2049 are also not recommended.
*)
let is_safe_char = function
  | '\033' .. '\060'
  | '\062' .. '\126' -> true
  | _                -> false

(* See RFC 2045 § 6.7:

   hex-octet       := "=" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
                        ; Octet must be used for characters > 127, =,
                        ; SPACEs or TABs at the ends of lines, and is
                        ; recommended for any character not listed in
                        ; RFC 2049 as "mail-safe".
*)
let is_hex_octet = function
  | 'A' .. 'F' -> true
  | chr        -> Rfc822.is_digit chr

let p_hex_octet p state =
  Lexer.p_chr '=' state;

  let s = Lexer.p_repeat ~a:2 ~b:2 is_hex_octet state in
  p (F.hex s) state

(* See RFC 2045 § 6.7:

   ptext           := hex-octet / safe-char
*)
let p_ptext p state =
  let buf = Buffer.create 16 in

  let rec loop state =
    match Lexer.cur_chr state with
    | '=' -> p_hex_octet (fun chr state -> Buffer.add_char buf chr; loop state) state
    | chr when is_safe_char chr ->
      Buffer.add_char buf chr;
      loop state
    | chr -> raise (Lexer.Error (Lexer.err_unexpected chr state))
  in

  loop state

(* See RFC 2045 § 6.7:

   qp-section      := [*(ptext / SPACE / TAB) ptext]
*)
let p_qp_section p state =
  let rec loop acc =
    Lexer.p_try_rule (fun data -> loop (data :: acc))
      (fun state ->
       match Lexer.cur_chr state with
       | '\x20' | '\x09' ->
         Lexer.junk_chr state;
         loop acc state
       | chr -> p (String.concat " " @@ List.rev acc) state)
      (p_ptext (fun data state -> `Ok (data, state)) state)
  in

  loop [] state

(* See RFC 2045 § 6.7:

   qp-segment      := qp-section *(SPACE / TAB) "="
                        ; Maximum length of 76 characters
*)
let p_qp_segment p state =
  p_qp_section
    (fun s state ->
     let _ = Lexer.p_repeat (function '\x20' | '\x09' -> true | _ -> false) in
     Lexer.p_chr '=' state;
     p s state)
    state

(* See RFC 2045 § 6.7:

   qp-part         := qp-section
                        ; Maximum length of 76 characters
*)
let p_qp_part = p_qp_section

(* See RFC 2045 § 6.7:

   transport-padding := *LWSP-char
                          ; Composers MUST NOT generate
                          ; non-zero length transport
                          ; padding, but receivers MUST
                          ; be able to handle padding
                          ; added by message transports.

*)
let p_transport_padding p state =
  let _ = Lexer.p_repeat Rfc822.is_lwsp state in
  p state

(* See RFC 2045 § 6.7:

   qp-line         := *(qp-segment transport-padding CRLF)
                      qp-part transport-padding
*)
let p_qp_line p state =
  let rec loop acc =
    Lexer.p_try_rule
      (fun seg -> loop (seg :: acc))
      (p_qp_part (fun seg -> p_transport_padding (p (List.rev @@ seg :: acc))))
      (p_qp_segment
       (fun seg ->
        p_transport_padding
          (fun state ->
           Lexer.p_chr '\r' state;
           Lexer.p_chr '\n' state;
           `Ok (seg, state))))
  in

  loop [] state

(* See RFC 2045 § 6.7:

   quoted-printable := qp-line *(CRLF qp-line)
*)
let p_quoted_printable p state =
  let rec loop acc =
    Lexer.p_try_rule
      (fun line -> loop (line :: acc))
      (p (List.rev acc))
      (fun state ->
       Lexer.p_chr '\r' state;
       Lexer.p_chr '\n' state;

       p_qp_line (fun line state -> `Ok (line, state)) state)
  in

  p_qp_line (fun line -> loop [line]) state

let p_decode stop p state =
  let buf = Buffer.create 16 in

  let rec decode state =
    let continue, state = stop state in
    if not continue
    then p (`QuotedPrintable (Buffer.contents buf)) state
    else match Lexer.cur_chr state with
    | '=' ->
      Lexer.p_chr '=' state;

      if Lexer.p_try is_hex_octet state = 2
      then begin
        let s = Lexer.p_repeat ~a:2 ~b:2 is_hex_octet state in

        F.add_char buf (F.hex s);
        decode state
      end else begin
        Lexer.p_chr '\r' state;
        Lexer.p_chr '\n' state;

        decode state
      end
    | '\x20' | '\x09' ->
      let lwsp = Lexer.p_while Rfc822.is_lwsp state in

      if Lexer.cur_chr state = '\r'
      then begin
        Lexer.p_chr '\r' state;
        Lexer.p_chr '\n' state;

        F.add_newline buf;
        decode state
      end else begin
        F.add_string buf lwsp;
        decode state
      end

    | chr when is_safe_char chr  ->
      Lexer.junk_chr state;
      F.add_char buf chr;
      decode state

    | chr -> raise (Lexer.Error (Lexer.err_unexpected chr state))
  in

  decode state

let p_encode stop p state =
  let buf = Buffer.create 16 in

  let rec encode qp state =
    let continue, state = stop state in
    if not continue
    then p (`QuotedPrintable (Buffer.contents buf)) state
    else match Lexer.cur_chr state with
    | '\x20' | '\x09' as chr ->
      Lexer.junk_chr state;

      if Lexer.cur_chr state = '\n'
      then begin
        T.add_newline buf qp (Some chr);
        Lexer.junk_chr state;
        encode qp state
      end else begin
        T.add_wsp buf qp chr;
        encode qp state
      end
    | chr when is_safe_char chr ->
      T.add_char buf qp chr;
      Lexer.junk_chr state;
      encode qp state
    | chr ->
      T.add_quoted_char buf qp chr;
      Lexer.junk_chr state;
      encode qp state
  in

  encode (T.make ()) state
