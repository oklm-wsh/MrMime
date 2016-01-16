{
  exception Lexical_error
}

(** {RFC 822} *****************************************************************)

(** See RFC 822 § 3.3:
                              ; (  Octal, Decimal. )
    CTL = <any ASCII control  ; (  0- 37,  0.- 31. )
           character and DEL> ; (    177,     127. )
*)
let rfc822_ctls      = ['\000' - '\031'] | '\127'

(** See RFC 822 § APPENDIX D

    DIGIT     =  <any ASCII decimal digit>    ; ( 60- 71, 48.- 57. )
*)
let rfc822_digit     = ['0' - '9']

(** RFC 822 § 3.3:
                                              ; (  Octal, Decimal. )
    SPACE     =  <ASCII SP, space>            ; (     40,      32. )
    HTAB      =  <ASCII HT, horizontal-tab>   ; (     11,       9. )

    LWSP-char =  SPACE / HTAB                 ; semantics = SPACE

    CR        =  <ASCII CR, carriage return>  ; (     15,      13. )
    LF        =  <ASCII LF, linefeed>         ; (     12,      10. )
    CRLF      =  CR LF
*)
let rfc822_space     = "\032"
let rfc822_htab      = "\009"
let rfc822_lwsp_char = rfc822_space | rfc822_htab
let rfc822_cr        = "\013"
let rfc822_lf        = "\010"
let rfc822_crlf      = rfc822_cr rfc822_lf

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
let rfc822_quoted_pair = '\\' ['\000' - '\127']
let rfc822_linear_white_space =  (rfc822_crlf ? rfc822_lwsp_char) +
let rfc822_ctext =
  (* XXX: [^ '(' ')' …] | linear_white_space
          or [^ '(' ')' … linear_white_space] *)
  [^ '(' ')' '\\' '\013'] | rfc822_linear_white_space

(** See RFC 822 § 3.4.5:

    Where permitted (i.e.,  in  words in structured fields)  quoted- strings are
    treated as a single symbol.  That is,  a  quoted- string is equivalent to an
    atom,  syntactically.  If a  quoted-string is to  be "folded"  onto multiple
    lines,  then the syntax  for folding must be adhered  to.  (See the "Lexical
    Analysis of Messages" section on "Folding Long Header Fields" above, and the
    section on "Case Independence" below.) Therefore,  the official semantics do
    not "see"  any bare  CRLFs that  are in  quoted-strings;  however particular
    parsing programs  may wish to  note their presence.  For  such programs,  it
    would be  reasonable to interpret a  "CRLF LWSP-char" as being  a CRLF which
    is part of the quoted-string;  i.e.,  the CRLF  is kept and the LWSP-char is
    discarded.  Quoted CRLFs (i.e.,  a backslash followed  by a CR followed by a
    LF) are also  subject to rules of folding,  but the  presence of the quoting
    character  (backslash) explicitly  indicates that  the CRLF  is data  to the
    quoted  string.   Stripping  off  the  first  following  LWSP-char  is  also
    appropriate when parsing quoted CRLFs.

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
let rfc822_qtext =
  rfc822_linear_white_space | ['\000' - '\033'] | ['\035' - '\091'] | ['\096' - '\127']
(** XXX: ignore or accept 0 .. 32 (controls characters) ? *)
let rfc822_quoted_string = '"' (rfc822_qtext | rfc822_quoted_pair) '"'

(** {RFC 2045} ****************************************************************)

(** See RFC 2045 § 5.1:

    tspecials :=  "(" / ")" / "<" / ">" / "@" /
                  "," / ";" / ":" / "\" / <">
                  "/" / "[" / "]" / "?" / "="
                  ; Must be in quoted-string,
                  ; to use within parameter values
*)
let rfc2045_tspecials =
  '(' | ')' | '<' | '>' | '@' | ',' | ';' |
  ':' | '=' | '"' | '/' | '[' | ']' | '?' | '\\'

(** See RFC 2045 § 5.1:

    token := 1* <any (US-ASCII) CHAR excepts SPACE, CTLs or tspecials>

    XXX: [^ space ctls tspecials]
*)
let rfc2045_token = [^ ' ' '\000' - '\031' '\127'
                    '(' ')' '<' '>' '@' ',' ';'
                    ':' '=' '"' '/' '[' ']' '?' '\\'] +

(** See RFC 2048 § 2.1.1:

    Media types  in the IETF  tree are normally  denoted by  names that  are not
    explicitly faceted, i.e., do not contain period (".", full stop) characters.

    See RFC 2045 § 5.1:

    ietf-token := <An extension token defined by a
                   standards-track RFC and registered
                   with IANA.>
*)
let rfc2045_period = '.'
(** XXX: [^ rfc822_space rfc822_ctls rfc2045_tspecials rfc2045_period] + *)
let rfc2045_ietf_token = [^ ' ' '\000' - '\031' '\127'
                         '(' ')' '<' '>' '@' ',' ';'
                         ':' '=' '"' '/' '[' ']' '?' '\\'
                         '.'] +

(** See RFC 2045 § 5.1:

    x-token := <The two characters "X-" or "x-" followed, with
                no intervening white space, by any token>

    extension-token := ietf-token / x-token
*)
let rfc2045_x_token = ("x-" | "X-") rfc2045_token
let rfc2045_extension_token = rfc2045_x_token | rfc2045_ietf_token

(** See RFC 2045 § 5.1:

    type := discrete-type / composite-type

    discrete-type  := "text" / "image" / "audio" / "video" /
                      "application" / extension-token

    composite-type := "message" / "multipart" /
                      extension-token
*)
let rfc2045_discrete_ty =
  "text" | "image" | "audio" | "video" | "application" |
  rfc2045_extension_token

let rfc2045_composite_ty =
  "message" | "multipart" |
  rfc2045_extension_token

let rfc2045_ty = rfc2045_discrete_ty | rfc2045_composite_ty

(** XXX: must be registered with IANA, see RFC 2048:

    iana-token := <A publicly-defined extension token. Tokens
                   of this form must be registered with IANA
                   as specified in RFC 2048.>
*)
let rfc2045_iana_token = rfc2045_token

(** See RFC 2045 § 5.3.1:

    There are,  therefore,  two  acceptable  mechanisms  for  defining new media
    subtypes:

     (1) Private values (starting with  "X-") may be defined bilaterally between
     two cooperating  agents  without  outside  registration or standardization.
     Such values cannot be registered or standardized.

     (2) New standard values should be  registered with IANA as described in RFC
     2048.

    subtype := extension-token / iana-token
*)
let rfc2045_subty = rfc2045_extension_token | rfc2045_iana_token

let rfc2045_attribute = rfc2045_token
let rfc2045_value = rfc2045_token | rfc822_quoted_string
(** XXX: quoted_string is useless (it does not concern the lexing but
         also parsing) ! It's handled below. *)

let rfc2045_mechanism =
  "7bit" | "8bit" | "binary" | "quoted-printable" | "base64"
  | rfc2045_x_token | rfc2045_ietf_token

(** {RFC 2822 & RFC 822} **************************************************)

(** See RFC 2822 § 2.2.2:

    … the space (SP,  ASCII value 32)  and horizontal tab (HTAB,  ASCII value 9)
    characters (together known as the white space characters, WSP) …

    XXX: Same as RFC 822 § 3.3.
*)
let rfc2822_space     = rfc822_space
let rfc2822_htab      = rfc822_htab
(** The previous name of wsp, in RFC 822, is LWSP_char *)
let rfc2822_wsp       = rfc822_space | rfc822_htab

(** See RFC 2822 § 3.2.1 or RFC 822 § 3.3:

    specials = "(" / ")"  / ; Special characters used in
               "<" / ">"  / ; other parts of the syntax
               "[" / "]"  /
               ":" / ";"  /
               "@" / %d92 /
               "," / "."  /
               DQUOTE

    XXX:  tspecials from RFC  2045 § 5.1 is not equivalent  with specials of RFC
    2822 § 3.2.1 (eg. specials has not "?", "=" or "/"). So,

      let specials = tspecials | '.'

    is wrong.
*)
let rfc2822_tspecials =
  '(' | ')' | '<' | '>' | '@' | ',' | ';' |
  ':' | '"' | '.' | '[' | ']' | '\\'

(** See RFC 2822 § 2.1 or part of RFC 822 § 3.3 *)
let rfc2822_cr        = rfc822_cr
let rfc2822_lf        = rfc822_lf
let rfc2822_crlf      = rfc822_crlf

(** Deliberately chose to include bare CR and LF here, although the RFC suggests
    them to  be included as part  of the  text characters.  The  rationale being
    that,  when parsing e-mail from a  text,  they will probably mean CRLF.  The
    issue should not arise in conforming e-mails.

    XXX: should be just [cr lf] *)
let rfc2822_crlf      = rfc2822_crlf | rfc2822_lf

(** See RFC 2822 § 4.2:

    In the obsolete  syntax,  any amount of folding white  space MAY be inserted
    where the obs-FWS  rule is allowed.  This creates the  possibility of having
    two consecutive "folds" in a line, and therefore the possibility that a line
    which makes  up a folded  header field could  be composed entirely  of white
    space.

    obs-FWS = 1*WSP *(CRLF 1*WSP)
*)
let rfc2822_obs_fws   = rfc2822_wsp + (rfc2822_crlf rfc2822_wsp +) *

(** See RFC 2822 § 4.1:

    The obs-char and obs-qp elements each add ASCII value 0.

    obs-qp   = %d92 (%d0-127)
    obs-char = %d0-9 / %d11 /  ; %d0-127 except CR and
               %d12 / %d14-127 ; LF
*)
let rfc2822_obs_qp    = '\\' ['\000' - '\127']
let rfc2822_obs_char  = ['\000' - '\009'] | '\011' | '\012' | ['\014' - '\127']

(** See RFC 2822 § 4.1:

    Bare CR and bare LF are added to obs-text and obs-utext.

    obs-text  = *LF *CR *(obs-char *LF *CR)
    obs-utext = obs-text
*)
let rfc2822_obs_text  =
  rfc2822_lf * rfc2822_cr * (rfc2822_obs_char rfc2822_lf * rfc2822_cr * ) *
let rfc2822_obs_utext = rfc2822_obs_text

(** Folding White Space, see RFC 2822 § 3.2.3

    FWS = ([*WSP CRLF] 1*WSP) /   ; Folding white space
          obs-FWS

    XXX: [ r ] in this regexp means an optional sequence (see RFC 2234 § 3.8)
*)
let rfc2822_fws       = ((rfc2822_wsp * rfc2822_crlf) ? rfc2822_wsp + ) | rfc2822_obs_fws

(** See RFC 2822 § 3.2.1:

    The following are  primitive tokens referred to elsewhere  in this standard,
    but  not otherwise  defined in  [RFC2234].  Some  of  them  will  not appear
    anywhere else in  the syntax,  but they are convenient to  refer to in other
    parts of this document.

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
let rfc2822_no_ws_ctl = ['\001' - '\008'] | '\011' | '\012' | ['\014' - '\031'] | '\127'
let rfc2822_text = ['\001' -'\009'] | '\011' | '\012' | ['\014' - '\127'] | rfc2822_obs_text

(** See RFC 2822 § 3.2.2:

    Some characters are reserved for special interpretation,  such as delimiting
    lexical tokens.  To permit use of these characters as uninterpreted data,  a
    quoting mechanism is provided.

    quoted-pair = (%d92 text) / obs-qp
*)
let rfc2822_quoted_pair = ('\\' rfc2822_text) | rfc2822_obs_qp

(** See RFC 2822 § 3.2.3:

    ctext = NO-WS-CTL /     ; Non white space controls
            %d33-39 /       ; The rest of the US-ASCII
            %d42-91 /       ;  characters not including "(",
            %d93-126        ;  ")", or "\""
*)
let rfc2822_ctext = rfc2822_no_ws_ctl | ['\033' - '\039'] | ['\042' - '\091'] | ['\093' - '\126']

(** See RFC 822 § 3.3 (XXX: Used by RFC 2045)

    comment      =  "(" *(ctext / quoted-pair / comment) ")"

    See RFC 822 § 3.4.3:

    A comment is  a  set  of  ASCII  characters,  which  is enclosed in matching
    parentheses and  which is not  within a quoted-string  The comment construct
    permits message originators  to  add  text  which  will  be useful for human
    readers, but which will be ignored by the formal semantics.  Comments should
    be retained while the message is subject to interpretation according to this
    standard.  However,  comments must NOT be  included in other cases,  such as
    during protocol exchanges with mail servers.

    Comments nest,  so that if an unquoted  left parenthesis occurs in a comment
    string, there must also be a matching right parenthesis. When a comment acts
    as the  delimiter between a sequence  of two  lexical symbols,  such  as two
    atoms,  it is lexically equivalent with a single SPACE,  for the purposes of
    regenerating the  sequence,  such as when  passing the sequence  onto a mail
    protocol server.  Comments are detected as  such only within field-bodies of
    structured fields.

    If a  comment is to  be "folded" onto  multiple lines,  then the  syntax for
    folding must be adhered to.  (See the "Lexical Analysis of Messages" section
    on  "Folding  Long   Header  Fields"  above,   and  the   section  on  "Case
    Independence" below.) Note  that  the  official  semantics  therefore do not
    "see" any unquoted CRLFs that are in comments, although particular pars- ing
    programs may wish to note  their presence.  For these programs,  it would be
    reasonable to interpret a  "CRLF LWSP-char" as being a CRLF  that is part of
    the comment;  i.e., the CRLF is kept and the LWSP-char is discarded.  Quoted
    CRLFs (i.e.,  a backslash followed  by a CR followed by a  LF) still must be
    followed by at least one LWSP-char.
*)
rule rfc822_comment level = parse
  | '('                 { rfc822_comment (level + 1) lexbuf }
  | ')'
    { if level <= 1 then (assert (level = 1); lexbuf)
      else rfc822_comment (level - 1) lexbuf }
  | rfc822_ctext | rfc822_quoted_pair { rfc822_comment level lexbuf }
  | _ as chr            { raise Lexical_error }

(** XXX: quoted-string like string from ocaml with respect RFC 822 *)
and  rfc822_quoted_string buffer = parse
  | rfc822_qtext as t
    { Buffer.add_string buffer t;
      rfc822_quoted_string buffer lexbuf }
  | rfc822_quoted_pair as t
    { Buffer.add_string buffer t;
      rfc822_quoted_string buffer lexbuf }
  | '"'
    {  Buffer.contents buffer }
  | _ { raise Lexical_error }

(** See RFC 822 § 3.3 or RFC 2045 § 5.1:

    In  addition,  comments are  allowed in  accordance with  RFC 822  rules for
    structured header fields. Thus the following two forms

      Content-type: text/plain; charset=us-ascii (Plain text)

      Content-type: text/plain; charset="us-ascii"

    are completely equivalent.
*)

and  rfc2045_content_type = parse
  | rfc2045_ty as t        { Parser.ATOM t }
  | '/'                    { Parser.SLASH }
  | rfc2045_subty as t     { Parser.ATOM t }
  | ';'                    { Parser.SEMICOLON }
  | rfc2045_attribute as t { Parser.ATOM t }
  | '='                    { Parser.EQUAL }
  | '"'
    { Parser.STRING (rfc822_quoted_string (Buffer.create 16) lexbuf) }
  (** XXX: handle of quoted-string. *)
  | rfc2045_value as v     { Parser.ATOM v }
  | rfc822_linear_white_space
    { rfc2045_content_type lexbuf }
  (** XXX: See RFC 822 § 3.1.4:

      To  aid  in  the creation  and  reading  of  structured  fields,  the free
      insertion  of linear-white-space  (which permits  folding by  inclusion of
      CRLFs) is allowed between lexical tokens. Rather than obscuring the syntax
      specifications for these  structured fields with explicit  syntax for this
      linear-white-  space,  the  existence  of  another  "lexical"  analyzer is
      assumed.  This analyzer does not apply  for unstructured field bodies that
      are simply strings of text,  as described above.  The analyzer provides an
      interpretation of the  unfolded text composing the body of  the field as a
      sequence of lexical sym- bols.
  *)
  | '('            { rfc2045_content_type (rfc822_comment 1 lexbuf) }
  (** XXX: handle of comment (RFC 822). *)
  | eof            { Parser.EOF }
  | _              { raise Lexical_error }

and  rfc2045_mechanism = parse
  | "7bit"                   { `Bit7 }
  | "8bit"                   { `Bit8 }
  | "binary"                 { `Binary }
  | "quoted-primary"         { `Quoted_primary }
  | "base64"                 { `Base64 }
  | rfc2045_x_token as t     { `X_token t }
  | rfc2045_ietf_token as t  { `Ietf_token t }
  | _                        { raise (Invalid_argument "Lexer.mechanism") }

(** See RFC 2045 § 4

    Since it is possible that a  future document might extend the message format
    standard again,  a formal  BNF is given for the  content of the MIME-Version
    field:

    version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT
*)
and  rfc2045_version = parse
  | rfc822_digit as digit     { Parser.DIGIT  (int_of_string (String.make 1 digit)) }
  | rfc822_linear_white_space { rfc2045_version lexbuf }
  | '.'                       { Parser.DOT }

  (** XXX: See RFC 2045 § 4:

      When checking  MIME-Version values  any RFC 822  comment strings  that are
      present must be ignored.
  *)
  | '('                       { rfc2045_version (rfc822_comment 1 lexbuf) }
  | eof                       { Parser.EOF }
  | _                         { raise Lexical_error }

(** See RFC 2822 § 3.2.3:

    Strings of  characters enclosed  in parentheses  are considered  comments so
    long as they do not appear  within a "quoted-string",  as defined in section
    3.2.5. Comments may nest.

    Comments can't be expressed with regular expressions, as they're nested:

    ccontent = ctext / quoted-pair / comment
    comment  = "(" *([FWS] ccontent) [FWS] ")"
*)
and  rfc2822_comment level = parse
  | '('                 { rfc2822_comment (level + 1) lexbuf }
  | rfc2822_quoted_pair | rfc2822_ctext { rfc2822_comment level lexbuf }
  | ')'
    { if level <= 1 then (assert (level = 1); lexbuf)
      else rfc2822_comment (level - 1) lexbuf }
  | _                   { rfc2822_comment level lexbuf }
