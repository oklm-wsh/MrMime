{
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
}

let hex        = ['0' - '9'] | ['a' - 'f'] | ['A' - 'F']
let strict_hex = ['0' - '9'] | ['A' - 'F']

(** See RFC 2045 § 6.7:

    safe-char := <any octet with decimal value of 33 through
                  60 inclusive, and 62 through 126>
                  ; Characters not listed as "mail-safe" in
                  ; RFC 2049 are also not recommended.
*)
let safe_char  = ['\033' - '\060'] | ['\062' - '\126']

let space      = '\032'
let htab       = '\009'
(** The previous name of wsp, in RFC 822, is LWSP_char.
    See RFC 2045 § 6.7:

    qp-section := [*(ptext / SPACE / TAB) ptext]
*)
let wsp        = space | htab

let cr         = '\013'
let lf         = '\010'
let crlf       = (cr lf) | lf
(* from CRLF of RFC 822 (see lexer.ml)
   with support of non-conforming e-mails *)

rule decode buf = parse
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
  | wsp * crlf
    { F.add_newline buf;
      decode buf lexbuf }

  (** See RFC 2045 § 6.7:

      qp-segment := qp-section *(SPACE / TAB) "="
                    ; Maximum length of 76 characters
  *)
  | '=' crlf
    { decode buf lexbuf }

  (** See RFC 2045 § 6.7:

      hex-octet := "=" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
                   ; Octet must be used for characters > 127, =,
                   ; SPACEs or TABs at the ends of lines, and is
                   ; recommended for any character not listed in
                   ; RFC 2049 as "mail-safe".
  *)
  | '=' (hex as a) (hex as b)
    { F.add_char buf
        (F.hex a b);
      decode buf lexbuf }

  (** See RFC 2045 § 6.7:

      qp-section := [*(ptext / SPACE / TAB) ptext]
  *)
  | (safe_char | wsp) as chr
    { F.add_char buf chr;
      decode buf lexbuf }
  | eof { () }
  | _ as chr
    (* XXX: should not appear in a quoted-printable body, but the most robus
     * way to handle them is probably copying them verbatim. *)
    { F.add_char buf chr;
      decode buf lexbuf }

and  encode buf acc = parse
  | (wsp as chr) ? '\n'
    { T.add_newline buf acc chr;
      encode buf acc lexbuf }

  | wsp as chr
    { T.add_wsp buf acc chr;
      encode buf acc lexbuf }

  | safe_char as chr
    { T.add_char buf acc chr;
      encode buf acc lexbuf }

  | eof { T.flush buf acc }
  | _ as chr
    { T.add_quoted_char buf acc chr;
      encode buf acc lexbuf }

{
  let encode buf lexbuf =
    encode buf (T.make ()) lexbuf
}
