module type SEDLEXING =
  sig
    val from_stream  : char Stream.t -> Sedlexing.lexbuf
    val from_channel : in_channel -> Sedlexing.lexbuf
    val from_string  : string -> Sedlexing.lexbuf
    val lexeme       : Sedlexing.lexbuf -> string
    val sub_lexeme   : Sedlexing.lexbuf -> int -> int -> string
  end

module Make (S : SEDLEXING) =
  struct
    exception Lexical_error

    let from_string  = S.from_string
    let from_stream  = S.from_stream
    let from_channel = S.from_channel
    let lexeme       = S.lexeme
    let sub_lexeme   = S.sub_lexeme

    let pos_fname = ref "<dummy>"
    let pos_lnum  = ref 1
    let pos_bol   = ref 0

    let pos pos_cnum =
      let open Lexing in
      {
        pos_fname = !pos_fname;
        pos_lnum  = !pos_lnum;
        pos_bol   = !pos_bol;
        pos_cnum;
      }

    let init fname =
      pos_fname := fname;
      pos_lnum  := 1;
      pos_bol   := 0

    let start_pos lexbuf = pos (Sedlexing.lexeme_start lexbuf)
    let end_pos   lexbuf = pos (Sedlexing.lexeme_end lexbuf)

    let locate lexbuf token =
      (token, start_pos lexbuf, end_pos lexbuf)

    let break_line lexbuf =
      pos_lnum := !pos_lnum + 1;
      pos_bol  := Sedlexing.lexeme_end lexbuf

    (** {RFC 822} *************************************************************)

    (** See RFC 822 § 3.3:
                                  ; (  Octal, Decimal. )
        CTL = <any ASCII control  ; (  0- 37,  0.- 31. )
               character and DEL> ; (    177,     127. )
    *)
    let ctls      = [%sedlex.regexp? 0 .. 31 | 127]

    (** RFC 822 § 3.3:
                                                  ; (  Octal, Decimal. )
        SPACE     =  <ASCII SP, space>            ; (     40,      32. )
        HTAB      =  <ASCII HT, horizontal-tab>   ; (     11,       9. )

        LWSP-char =  SPACE / HTAB                 ; semantics = SPACE

        CR        =  <ASCII CR, carriage return>  ; (     15,      13. )
        LF        =  <ASCII LF, linefeed>         ; (     12,      10. )
        CRLF      =  CR LF
    *)
    let space     = [%sedlex.regexp? 32]
    let htab      = [%sedlex.regexp? 9]
    let lwsp_char = [%sedlex.regexp? space | htab]
    let cr        = [%sedlex.regexp? 13]
    let lf        = [%sedlex.regexp? 10]
    let crlf      = [%sedlex.regexp? cr, lf]

    (** See RFC 822 § 3.3 (XXX: Obsolete version):

                                                   ; (  Octal, Decimal. )
        BS          = <ASCII BS, backslash>        ; (    134,      92. )
        linear-white-space = 1*([CRLF] LWSP-char)  ; semantics = SPACE
                                                   ; CRLF => folding
        quoted-pair =  BS CHAR                     ; may quote any char
        ctext       =  <any CHAR excluding "(",    ; => may be folded
                        ")", BS & CR, & including
                        linear-white-space>
    *)
    let quoted_pair = [%sedlex.regexp? '\\', 0 .. 127 ]
    let linear_white_space = [%sedlex.regexp? Plus (Opt crlf, lwsp_char)]
    let ctext = [%sedlex.regexp?
      (* XXX: [^ '(' ')' …] | linear_white_space
              or [^ '(' ')' … linear_white_space] *)
      Compl ('(' | ')' | '\\' | 13) | linear_white_space ]

    (** See RFC 822 § 3.3 (XXX: Used by RFC 2045)

        comment      =  "(" *(ctext / quoted-pair / comment) ")"

        See RFC 822 § 3.4.3:

        A comment is  a set of ASCII characters,  which  is enclosed in matching
        parentheses  and  which  is  not  within  a  quoted-string  The  comment
        construct permits message  originators to add text which  will be useful
        for human readers,  but  which will be ignored by  the formal semantics.
        Comments  should   be  retained   while  the   message  is   subject  to
        interpretation according to this standard. However, comments must NOT be
        included in  other cases,  such as  during protocol exchanges  with mail
        servers.

        Comments  nest,  so that  if an  unquoted left  parenthesis occurs  in a
        comment string, there must also be a matching right parenthesis.  When a
        comment acts as the delimiter between a sequence of two lexical symbols,
        such as two atoms,  it is lexically equivalent with a single SPACE,  for
        the purposes  of regenerating  the sequence,  such  as when  passing the
        sequence onto a mail protocol server. Comments are detected as such only
        within field-bodies of structured fields.

        If a comment is to be "folded" onto multiple lines,  then the syntax for
        folding must  be adhered  to.  (See the  "Lexical Analysis  of Messages"
        section on "Folding Long Header Fields" above,  and the section on "Case
        Independence" below.) Note that the  official semantics therefore do not
        "see" any unquoted CRLFs that are in comments, although particular pars-
        ing programs may  wish to note their  presence.  For these programs,  it
        would be reasonable to interpret a "CRLF LWSP-char" as being a CRLF that
        is part  of the comment;  i.e.,  the CRLF  is kept and  the LWSP-char is
        discarded.  Quoted CRLFs (i.e., a backslash followed by a CR followed by
        a LF) still must be followed by at least one LWSP-char.
    *)
    let rec comment ?(level = 0) lexbuf =
      match%sedlex lexbuf with
      | '(' -> comment ~level:(level + 1) lexbuf
      | ')' ->
        if level <= 1 then (assert (level = 1); lexbuf)
        else comment ~level:(level - 1) lexbuf
      | quoted_pair | ctext -> comment ~level lexbuf
      | _ -> raise Lexical_error

    (** See RFC 822 § 3.4.5:

        Where permitted  (i.e.,  in words in structured  fields) quoted- strings
        are treated as a single symbol.  That is, a quoted- string is equivalent
        to an atom,  syntactically.  If  a quoted-string is to  be "folded" onto
        multiple lines, then the syntax for folding must be adhered to. (See the
        "Lexical Analysis of  Messages" section on "Folding  Long Header Fields"
        above,  and the  section on "Case  Independence" below.) Therefore,  the
        official  semantics   do  not   "see"  any  bare   CRLFs  that   are  in
        quoted-strings; however particular parsing programs may wish to note their
        presence. For such programs, it would be reasonable to interpret a "CRLF
        LWSP-char" as being a CRLF which is part of the quoted-string; i.e., the
        CRLF is kept and the LWSP-char is discarded. Quoted CRLFs (i.e., a backslash
        followed by a CR followed by a LF) are also subject to rules of folding, but
        the presence of the quoting character (backslash) explicitly indicates that
        the CRLF is data to the quoted string. Stripping off the first following
        LWSP-char is also appropriate when parsing quoted CRLFs.

        See RFC 822 § APPENDIX D

                                                      ; (  Octal, Decimal. )
        DQUOTE        = <ASCII '"', double-quote>     ; (     42,      34. )
        qtext         = <any CHAR excepting <DQUOTE>, ; => may be folded
                         BS & CR, and including
                         linear-white-space>
        quoted-pair   = BS CHAR                       ; may quote any char
        quoted-string = <"> *(qtext/quoted-pair) <">  ; Regular qtext or
                                                      ;  quoted chars.
    *)
    let qtext = [%sedlex.regexp?
      linear_white_space | 0 .. 33 | 35 .. 91 | 96 .. 127 ]
    (** XXX: ignore or accept 0 .. 32 (controls characters) ? *)
    let quoted_string = [%sedlex.regexp? '"', (qtext | quoted_pair), '"']

    (** XXX: quoted-string like string from ocaml with respect RFC 822 *)
    let rec quoted_string buffer lexbuf = match%sedlex lexbuf with
      | qtext ->
        Buffer.add_string buffer (S.lexeme lexbuf);
        quoted_string buffer lexbuf
      | quoted_pair ->
        Buffer.add_string buffer (S.lexeme lexbuf);
        quoted_string buffer lexbuf
      | '"' ->
        Buffer.contents buffer
      | _ -> raise Lexical_error

    (** {RFC 2045} ************************************************************)

    (** See RFC 2045 § 5.1:

        tspecials :=  "(" / ")" / "<" / ">" / "@" /
                      "," / ";" / ":" / "\" / <">
                      "/" / "[" / "]" / "?" / "="
                      ; Must be in quoted-string,
                      ; to use within parameter values
    *)
    let tspecials = [%sedlex.regexp?
      '(' | ')' | '<' | '>' | '@' | ',' | ';' |
      ':' | '=' | '"' | '/' | '[' | ']' | '?' | '\\']

    (** See RFC 2045 § 5.1:

        token := 1* <any (US-ASCII) CHAR excepts SPACE, CTLs or tspecials>
    *)
    let token = [%sedlex.regexp?
      Plus (Compl (space | (* RFC822. *)ctls | tspecials)) ]

    (** See RFC 2048 § 2.1.1:

        Media types in the IETF tree are  normally denoted by names that are not
        explicitly  faceted,  i.e.,  do  not  contain  period  (".",  full stop)
        characters.

        See RFC 2045 § 5.1:

        ietf-token := <An extension token defined by a
                       standards-track RFC and registered
                       with IANA.>
    *)
    let period = [%sedlex.regexp? '.']
    let ietf_token = [%sedlex.regexp?
      Plus (Compl (space | ctls | tspecials | period)) ]

    (** See RFC 2045 § 5.1:

        x-token := <The two characters "X-" or "x-" followed, with
                    no intervening white space, by any token>

        extension-token := ietf-token / x-token
    *)
    let x_token = [%sedlex.regexp? ("x-" | "X-"), token]
    let extension_token = [%sedlex.regexp? x_token | ietf_token ]

    (** See RFC 2045 § 5.1:

        type := discrete-type / composite-type

        discrete-type  := "text" / "image" / "audio" / "video" /
                          "application" / extension-token

        composite-type := "message" / "multipart" /
                          extension-token
    *)
    let discrete_ty = [%sedlex.regexp?
      "text" | "image" | "audio" | "video" | "application" |
      extension_token ]

    let composite_ty = [%sedlex.regexp?
      "message" | "multipart" |
      extension_token ]

    let ty = [%sedlex.regexp? discrete_ty | composite_ty]

    (** XXX: must be registered with IANA, see RFC 2048:

        iana-token := <A publicly-defined extension token. Tokens
                       of this form must be registered with IANA
                       as specified in RFC 2048.>
    *)
    let iana_token = [%sedlex.regexp? token]

    (** See RFC 2045 § 5.3.1:

        There are,  therefore,  two acceptable mechanisms for defining new media
        subtypes:

         (1) Private values  (starting  with  "X-")  may  be defined bilaterally
             between  two cooperating  agents  without  outside  registration or
             standardization. Such values cannot be registered or standardized.

         (2) New standard values should be  registered with IANA as described in
             RFC 2048.

        subtype := extension-token / iana-token
    *)
    let subty = [%sedlex.regexp? extension_token | iana_token]

    (** See RFC 822 § 3.3 or RFC 2045 § 5.1:

        In addition,  comments are allowed in  accordance with RFC 822 rules for
        structured header fields. Thus the following two forms

          Content-type: text/plain; charset=us-ascii (Plain text)

          Content-type: text/plain; charset="us-ascii"

        are completely equivalent.
    *)
    let comment = comment

    let attribute = [%sedlex.regexp? token]
    let value = [%sedlex.regexp? token | quoted_string]
    (** XXX: quoted_string is useless (it does not concern the lexing but
             also parsing) ! It's handled below. *)

    let rec content_type lexbuf = match%sedlex lexbuf with
      | ty            -> locate lexbuf (Parser.ATOM (S.lexeme lexbuf))
      | '/'           -> locate lexbuf (Parser.SLASH)
      | subty         -> locate lexbuf (Parser.ATOM (S.lexeme lexbuf))
      | ';'           -> locate lexbuf (Parser.SEMICOLON)
      | attribute     -> locate lexbuf (Parser.ATOM (S.lexeme lexbuf))
      | '='           -> locate lexbuf (Parser.EQUAL)
      | '"'           ->
        locate lexbuf (Parser.STRING (quoted_string (Buffer.create 16) lexbuf))
      (** XXX: handle of quoted-string. *)
      | value         -> locate lexbuf (Parser.ATOM (S.lexeme lexbuf))
      | linear_white_space ->
        content_type lexbuf
      (** XXX: See RFC 822 § 3.1.4:

          To aid  in the  creation and reading  of structured  fields,  the free
          insertion of linear-white-space (which permits folding by inclusion of
          CRLFs) is  allowed between lexical tokens.  Rather  than obscuring the
          syntax specifications for these structured fields with explicit syntax
          for  this linear-white-  space,  the  existence  of  another "lexical"
          analyzer is  assumed.  This analyzer does  not apply  for unstructured
          field bodies that are simply strings of text, as described above.  The
          analyzer provides an interpretation of the unfolded text composing the
          body of the field as a sequence of lexical sym- bols.
      *)
      | '('           -> content_type (comment ~level:1 lexbuf)
      (** XXX: handle of comment (RFC 822). *)
      | eof           -> locate lexbuf (Parser.EOF)
      | _             -> raise Lexical_error

    let mechanism = [%sedlex.regexp?
      "7bit" | "8bit" | "binary" | "quoted-printable" | "base64"
      | x_token | ietf_token ]

    (** {RFC 2822 & RFC 822} **************************************************)

    (** See RFC 2822 § 2.2.2:

        … the space (SP,  ASCII value 32) and horizontal tab (HTAB,  ASCII value
        9) characters (together known as the white space characters, WSP) …

        XXX: Same as RFC 822 § 3.3.
    *)
    let space     = [%sedlex.regexp? (* RFC822. *)space]
    let htab      = [%sedlex.regexp? (* RFC822. *)htab]
    (** The previous name of wsp, in RFC 822, is LWSP_char *)
    let wsp       = [%sedlex.regexp? space | htab]

    (** See RFC 2822 § 3.2.1 or RFC 822 § 3.3:

        specials = "(" / ")"  / ; Special characters used in
                   "<" / ">"  / ; other parts of the syntax
                   "[" / "]"  /
                   ":" / ";"  /
                   "@" / %d92 /
                   "," / "."  /
                   DQUOTE

        XXX:  tspecials from RFC  2045 § 5.1 is not  equivalent with specials of
        RFC 2822 § 3.2.1 (eg. specials has not "?", "=" or "/"). So,

          let specials = [%sedlex.regexp? tspecials | '.']

        is wrong.
    *)
    let tspecials = [%sedlex.regexp?
      '(' | ')' | '<' | '>' | '@' | ',' | ';' |
      ':' | '"' | '.' | '[' | ']' | '\\']

    (** See RFC 2822 § 2.1 or part of RFC 822 § 3.3 *)
    let cr        = [%sedlex.regexp? (* RFC822. *)cr]
    let lf        = [%sedlex.regexp? (* RFC822. *)lf]
    let crlf      = [%sedlex.regexp? (* RFC822. *)crlf]

    (** Deliberately chose  to include  bare CR and  LF here,  although  the RFC
        suggests  them to  be included  as  part  of  the  text characters.  The
        rationale  being  that,  when parsing  e-mail  from  a  text,  they will
        probably mean CRLF. The issue should not arise in conforming e-mails.

        XXX: should be just [cr, lf] *)
    let crlf      = [%sedlex.regexp? crlf | lf]

    (** See RFC 2822 § 4.2:

        In  the obsolete  syntax,  any amount  of  folding  white  space  MAY be
        inserted where the obs-FWS rule is allowed. This creates the possibility
        of  having  two  consecutive  "folds"  in  a  line,  and  therefore  the
        possibility that  a line which makes  up a folded header  field could be
        composed entirely of white space.

        obs-FWS = 1*WSP *(CRLF 1*WSP)
    *)
    let obs_fws   = [%sedlex.regexp? Plus wsp, Star (crlf, Plus wsp)]

    (** See RFC 2822 § 4.1:

        The obs-char and obs-qp elements each add ASCII value 0.

        obs-qp   = %d92 (%d0-127)
        obs-char = %d0-9 / %d11 /  ; %d0-127 except CR and
                   %d12 / %d14-127 ; LF
    *)
    let obs_qp    = [%sedlex.regexp? '\\', 0 .. 127]
    let obs_char  = [%sedlex.regexp? 0 .. 9 | 11 | 12 | 14 .. 127]

    (** See RFC 2822 § 4.1:

        Bare CR and bare LF are added to obs-text and obs-utext.

        obs-text  = *LF *CR *(obs-char *LF *CR)
        obs-utext = obs-text
    *)
    let obs_text  = [%sedlex.regexp?
      Star lf, Star cr, (obs_char, Star lf, Star cr)]
    let obs_utext = [%sedlex.regexp? obs_text]

    (** Folding White Space, see RFC 2822 § 3.2.3

        FWS = ([*WSP CRLF] 1*WSP) /   ; Folding white space
              obs-FWS
    *)
    let fws       = [%sedlex.regexp? (Opt (Star wsp, crlf), Plus wsp) | obs_fws]

    (** See RFC 2822 § 3.2.1:

        The  following  are  primitive  tokens  referred  to  elsewhere  in this
        standard, but not otherwise defined in [RFC2234].  Some of them will not
        appear anywhere else in the syntax,  but they are convenient to refer to
        in other parts of this document.

        NO-WS-CTL = %d1-8 /         ; US-ASCII control characters
                    %d11 /          ;  that do not include the
                    %d12 /          ;  carriage return, line feed,
                    %d14-31 /       ;  and white space characters
                    %d127
        text      = %d1-9 /         ; Characters excluding CR and LF
                    %d11 /
                    %d12 /
                    %d14-127 /
                    obs-text

        specials  = (see below)
    *)
    let no_ws_ctl = [%sedlex.regexp? 1 .. 8 | 11 | 12 | 14 .. 31 | 127]
    let text = [%sedlex.regexp? 1 .. 9 | 11 | 12 | 14 .. 127 | obs_text ]

    (** See RFC 2822 § 3.2.2:

        Some  characters  are  reserved  for  special  interpretation,  such  as
        delimiting  lexical  tokens.  To  permit  use  of  these  characters  as
        uninterpreted data, a quoting mechanism is provided.

        quoted-pair = (%d92 text) / obs-qp
    *)
    let quoted_pair = [%sedlex.regexp? ('\\', text) | obs_qp]

    (** See RFC 2822 § 3.2.3:

        ctext = NO-WS-CTL /     ; Non white space controls
                %d33-39 /       ; The rest of the US-ASCII
                %d42-91 /       ;  characters not including "(",
                %d93-126        ;  ")", or "\""
    *)
    let ctext = [%sedlex.regexp? no_ws_ctl | 33 .. 39 | 42 .. 91 | 93 .. 126]

    (** See RFC 2822 § 3.2.3:

        Strings of characters enclosed in parentheses are considered comments so
        long  as they  do not  appear within  a "quoted-string",  as  defined in
        section 3.2.5. Comments may nest.

        Comments can't be expressed with regular expressions, as they're nested:

        ccontent = ctext / quoted-pair / comment
        comment  = "(" *([FWS] ccontent) [FWS] ")"
    *)
    let rec comment ?(level = 0) lexbuf = match%sedlex lexbuf with
      | '(' -> comment ~level:(level + 1) lexbuf
      | quoted_pair | ctext -> comment ~level lexbuf
      | ')' ->
        if level <= 1 then (assert (level = 1); lexbuf)
        else comment ~level:(level - 1) lexbuf
      | _ -> comment ~level lexbuf
  end
