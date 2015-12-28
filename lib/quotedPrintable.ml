module Make (S : Lexer.SEDLEXING) =
struct
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

    let add_char buf chr =
      Buffer.add_char buf chr
  end

  let hex        = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
  let strict_hex = [%sedlex.regexp? '0' .. '9' | 'A' .. 'F']

  (** See RFC 2045 § 6.7:

      safe-char := <any octet with decimal value of 33 through
                    60 inclusive, and 62 through 126>
                    ; Characters not listed as "mail-safe" in
                    ; RFC 2049 are also not recommended.
  *)
  let safe_char  = [%sedlex.regexp? 33 .. 60 | 62 .. 126]

  let space   = [%sedlex.regexp? 32]
  let htab    = [%sedlex.regexp? 9]
  (** The previous name of wsp, in RFC 822, is LWSP_char.
      See RFC 2045 § 6.7:

      qp-section := [*(ptext / SPACE / TAB) ptext]
  *)
  let wsp     = [%sedlex.regexp? space | htab]

  let cr      = [%sedlex.regexp? 13]
  let lf      = [%sedlex.regexp? 10]
  let crlf    = [%sedlex.regexp? cr, lf | lf]
  (* from CRLF of RFC 822 (see lexer.ml)
     with support of non-conforming e-mails *)

  let rec decode buf lexbuf = match%sedlex lexbuf with
    (** See RFC 2045 § 6.7:

        qp-line := *(qp-segment transport-padding CRLF)
                   qp-part transport-padding
        transport-padding := *LWSP-char
                             ; Composers MUST NOT generate
                             ; non-zero length transport
                             ; padding, but receivers MUST
                             ; be able to handle padding
                             ; added by message transports.
    *)
    | Star wsp, crlf ->
      F.add_newline buf;
      decode buf lexbuf

    (** See RFC 2045 § 6.7:

        qp-segment := qp-section *(SPACE / TAB) "="
                      ; Maximum length of 76 characters
    *)
    | '=', crlf ->
      decode buf lexbuf

    (** See RFC 2045 § 6.7:

        hex-octet := "=" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
                     ; Octet must be used for characters > 127, =,
                     ; SPACEs or TABs at the ends of lines, and is
                     ; recommended for any character not listed in
                     ; RFC 2049 as "mail-safe".
    *)
    | '=', hex, hex ->
      F.add_char buf
        (F.hex (S.sub_lexeme lexbuf 1 1
                |> fun s -> (* assert (String.length s = 1); *) String.get s 0)
               (S.sub_lexeme lexbuf 2 1
                |> fun s -> (* assert (String.length s = 1); *) String.get s 0));
      decode buf lexbuf

    (** See RFC 2045 § 6.7:

        qp-section := [*(ptext / SPACE / TAB) ptext]
    *)
    | safe_char | wsp ->
      F.add_char buf
        (S.lexeme lexbuf |> fun s -> (* assert (String.length s = 1)); *) String.get s 0)
    | eof -> ()
    | _ ->
      F.add_char buf
        (S.lexeme lexbuf |> fun s -> (* assert (String.length s = 1)); *) String.get s 0)
end
