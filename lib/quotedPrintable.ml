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
    in Char.chr ((aux (String.get s 0) * 16) + (aux (String.get s 1)))

  let add_char buf chr =
    Buffer.add_char buf chr

  let add_string buf str =
    Buffer.add_string buf str
end

module T =
struct
  let _to = "0123456789ABCDEF"

  type t =
    { word             : Buffer.t
    ; mutable position : int }

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

open BaseLexer

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

let p_hex_octet p =
  p_chr '='
  @ (2 * 2) is_hex_octet
  @ fun s -> p (F.hex s)

(* See RFC 2045 § 6.7:

   ptext           := hex-octet / safe-char
*)
let p_ptext p state =
  let buf = Buffer.create 16 in

  let rec loop () =
    cur_chr
    @ function
      | '=' ->
        p_hex_octet
        @ fun chr -> Buffer.add_char buf chr; loop ()
      | chr when is_safe_char chr ->
        fun state -> Buffer.add_char buf chr; loop () state
      | chr -> fun state -> raise (Error.Error (Error.err_unexpected chr state))
  in

  loop () state

(* See RFC 2045 § 6.7:

   qp-section      := [*(ptext / SPACE / TAB) ptext]
*)
let p_qp_section p state =
  let rec loop acc =
    (p_ptext (fun data state -> `Ok (data, state)))
    / (cur_chr @ function
       | '\x20' | '\x09' -> junk_chr @ loop acc
       | chr -> p (String.concat " " @@ List.rev acc))
    @ (fun data -> loop (data :: acc))
  in

  loop [] state

(* See RFC 2045 § 6.7:

   qp-segment      := qp-section *(SPACE / TAB) "="
                        ; Maximum length of 76 characters
*)
let p_qp_segment p =
  p_qp_section
  @ fun s -> (0 * 0) (function '\x20' | '\x09' -> true | _ -> false)
  @ fun _ -> p_chr '='
  @ p s

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
let p_transport_padding p =
  (0 * 0) Rfc822.is_lwsp
  @ fun _ -> p

(* See RFC 2045 § 6.7:

   qp-line         := *(qp-segment transport-padding CRLF)
                      qp-part transport-padding
*)
let p_qp_line p state =
  let rec loop acc =
    (p_qp_segment
     @ fun seg -> p_transport_padding
     @ p_chr '\r'
     @ p_chr '\n'
     @ fun state -> `Ok (seg, state))
    / (p_qp_part
       @ fun seg -> p_transport_padding
       @ p (List.rev @@ seg :: acc))
    @ (fun seg -> loop (seg :: acc))
  in

  loop [] state

(* See RFC 2045 § 6.7:

   quoted-printable := qp-line *(CRLF qp-line)
*)
let p_quoted_printable p =
  let rec loop acc =
    (p_chr '\r'
     @ p_chr '\n'
     @ p_qp_line
     @ fun line state -> `Ok (line, state))
    / (p (List.rev acc))
    @ (fun line -> loop (line :: acc))
  in

  p_qp_line @ fun line -> loop [line]

(* See RFC 2047 § 4.2:

   The   "Q"   encoding   is   similar   to   the   "Quoted-Printable"  content-
   transfer-encoding  defined  in  RFC  2045.  It  is  designed  to  allow  text
   containing mostly  ASCII characters to  be decipherable on  an ASCII terminal
   without decoding.

   (1) Any 8-bit value  may be represented by a "="  followed by two hexadecimal
       digits. For example, if the character set in use were ISO-8859-1, the "="
       character would thus be encoded  as "=3D",  and a SPACE by "=20".  (Upper
       case should be used for hexadecimal digits "A" through "F".)

   (2) The  8-bit  hexadecimal  value   20  (e.g.,   ISO-8859-1  SPACE)  may  be
       represented as "_" (underscore, ASCII 95.).  (This character may not pass
       through some internetwork mail gateways, but its use will greatly enhance
       readability of  "Q" encoded data with  mail readers  that do  not support
       this encoding.) Note that the "_" always represents hexadecimal 20,  even
       if  the  SPACE  character  occupies  a  different  code  position  in the
       character set in use.

   (3) 8-bit values  which correspond to  printable ASCII characters  other than
       "=",  "?",  and "_" (underscore), MAY be represented as those characters.
       (But see section 5 for  restrictions.) In particular,  SPACE and TAB MUST
       NOT be represented as themselves within encoded words.

*)
let p_inline_decode stop p state =
  let buf = Buffer.create 16 in

  let rec decode state =
    let rec aux = function
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err -> err
      | `Stop state -> p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr
         @ function
         | '=' ->
           junk_chr
           @ (2 * 2) is_hex_octet
           @ fun s ->
             F.add_char buf (F.hex s);
             decode
         | '_' ->
           junk_chr
           @ fun state -> Buffer.add_char buf ' '; decode state
         | '?' ->
           fun state -> raise (Error.Error (Error.err_unexpected '?' state))
         | chr ->
           junk_chr
           @ fun state -> Buffer.add_char buf chr; decode state)
        state
    in aux @@ safe stop state
  in

  decode state

(* See RFC 2045 § 6.7:

   The Quoted-Printable encoding is intended to represent data that
   largely consists of octets that correspond to printable characters in
   the US-ASCII character set.  It encodes the data in such a way that
   the resulting octets are unlikely to be modified by mail transport.
   If the data being encoded are mostly US-ASCII text, the encoded form
   of the data remains largely recognizable by humans.  A body which is
   entirely US-ASCII may also be encoded in Quoted-Printable to ensure
   the integrity of the data should the message pass through a
   character-translating, and/or line-wrapping gateway.

   In this encoding, octets are to be represented as determined by the
   following rules:

    (1)   (General 8bit representation) Any octet, except a CR or
          LF that is part of a CRLF line break of the canonical
          (standard) form of the data being encoded, may be
          represented by an "=" followed by a two digit
          hexadecimal representation of the octet's value.  The
          digits of the hexadecimal alphabet, for this purpose,
          are "0123456789ABCDEF".  Uppercase letters must be
          used; lowercase letters are not allowed.  Thus, for
          example, the decimal value 12 (US-ASCII form feed) can
          be represented by "=0C", and the decimal value 61 (US-
          ASCII EQUAL SIGN) can be represented by "=3D".  This
          rule must be followed except when the following rules
          allow an alternative encoding.

    (2)   (Literal representation) Octets with decimal values of
          33 through 60 inclusive, and 62 through 126, inclusive,
          MAY be represented as the US-ASCII characters which
          correspond to those octets (EXCLAMATION POINT through
          LESS THAN, and GREATER THAN through TILDE,
          respectively).

    (3)   (White Space) Octets with values of 9 and 32 MAY be
          represented as US-ASCII TAB (HT) and SPACE characters,
          respectively, but MUST NOT be so represented at the end
          of an encoded line.  Any TAB (HT) or SPACE characters
          on an encoded line MUST thus be followed on that line
          by a printable character.  In particular, an "=" at the
          end of an encoded line, indicating a soft line break
          (see rule #5) may follow one or more TAB (HT) or SPACE
          characters.  It follows that an octet with decimal
          value 9 or 32 appearing at the end of an encoded line
          must be represented according to Rule #1.  This rule is
          necessary because some MTAs (Message Transport Agents,
          programs which transport messages from one user to
          another, or perform a portion of such transfers) are
          known to pad lines of text with SPACEs, and others are
          known to remove "white space" characters from the end
          of a line.  Therefore, when decoding a Quoted-Printable
          body, any trailing white space on a line must be
          deleted, as it will necessarily have been added by
          intermediate transport agents.

    (4)   (Line Breaks) A line break in a text body, represented
          as a CRLF sequence in the text canonical form, must be
          represented by a (RFC 822) line break, which is also a
          CRLF sequence, in the Quoted-Printable encoding.  Since
          the canonical representation of media types other than
          text do not generally include the representation of
          line breaks as CRLF sequences, no hard line breaks
          (i.e. line breaks that are intended to be meaningful
          and to be displayed to the user) can occur in the
          quoted-printable encoding of such types.  Sequences
          like "=0D", "=0A", "=0A=0D" and "=0D=0A" will routinely
          appear in non-text data represented in quoted-
          printable, of course.

          Note that many implementations may elect to encode the
          local representation of various content types directly
          rather than converting to canonical form first,
          encoding, and then converting back to local
          representation.  In particular, this may apply to plain
          text material on systems that use newline conventions
          other than a CRLF terminator sequence.  Such an
          implementation optimization is permissible, but only
          when the combined canonicalization-encoding step is
          equivalent to performing the three steps separately.

    (5)   (Soft Line Breaks) The Quoted-Printable encoding
          REQUIRES that encoded lines be no more than 76
          characters long.  If longer lines are to be encoded
          with the Quoted-Printable encoding, "soft" line breaks
          must be used.  An equal sign as the last character on a
          encoded line indicates such a non-significant ("soft")
          line break in the encoded text.
*)
let p_decode stop p state =
  let buf = Buffer.create 16 in

  let rec decode state =
    let rec aux = function
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err -> err
      | `Stop state -> p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr @ function
         | '=' ->
           junk_chr
           @ p_try is_hex_octet
           @ fun n ->
             if n >= 2
             then (2 * 2) is_hex_octet
                  @ fun s ->
                      F.add_char buf (F.hex s);
                      decode
             else p_chr '\r'
                  @ p_chr '\n'
                  @ decode
         | '\x20' | '\x09' ->
           p_while Rfc822.is_lwsp
           @ fun lwsp -> cur_chr
           @ (function
              | '\r' -> p_chr '\r'
                        @ p_chr '\n'
                        @ fun state -> F.add_newline buf; decode state
              | chr -> F.add_string buf lwsp; decode)
         | '\r' ->
           p_chr '\r'
           @ p_chr '\n'
           @ fun state ->
             F.add_newline buf;
             decode state
         | chr when is_safe_char chr  ->
           F.add_char buf chr; junk_chr @ decode
         | chr -> fun state -> raise (Error.Error (Error.err_unexpected chr state)))
        state
    in aux (safe stop state)
  in

  decode state

let p_inline_encode stop p state =
  let buf = Buffer.create 16 in

  let rec encode state =
    let rec aux = function
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err -> err
      | `Stop state -> p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr @ function
         | '\x20' ->
           junk_chr
           @ fun state -> Buffer.add_char buf '_'; encode state
         | '\x09' ->
           junk_chr
           @ fun state -> Buffer.add_string buf "=09"; encode state
         | '?' ->
           junk_chr
           @ fun state -> Buffer.add_string buf "=3F"; encode state
         | '_' ->
           junk_chr
           @ fun state -> Buffer.add_string buf "=5F"; encode state
         | '=' ->
           junk_chr
           @ fun state -> Buffer.add_string buf "=3D"; encode state
         | chr when is_safe_char chr ->
           junk_chr
           @ fun state -> Buffer.add_char buf chr; encode state
         | chr ->
           junk_chr
           @ fun state ->
             let code = Char.code chr in
             let h    = (code lsr 4) land (16 - 1) in
             let l    =  code        land (16 - 1) in

             Buffer.add_char buf '=';
             Buffer.add_char buf T._to.[h];
             Buffer.add_char buf T._to.[l];

             encode state)
        state
    in aux @@ safe stop state
  in

  encode state

let p_encode stop p state =
  let buf = Buffer.create 16 in

  let rec encode qp state =
    let rec aux = function
      | #Error.err as r -> r
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | `Stop state -> p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr @ function
         | '\x20' | '\x09' as chr ->
           junk_chr
           @ cur_chr
           @ (function
              | '\n' ->
                T.add_newline buf qp (Some chr);
                junk_chr @ encode qp
              | chr ->
                T.add_wsp buf qp chr;
                encode qp)
         | chr when is_safe_char chr ->
           T.add_char buf qp chr;
           junk_chr @ encode qp
         | chr ->
           T.add_quoted_char buf qp chr;
           junk_chr @ encode qp)
        state
    in aux (stop state)
  in

  encode (T.make ()) state
