{
  exception Lexical_error
}

(** See RFC 822 § 3.3:
                              ; (  Octal, Decimal. )
    CTL = <any ASCII control  ; (  0- 37,  0.- 31. )
           character and DEL> ; (    177,     127. )
*)
let ctls      = ['\000' - '\031'] | '\127'

(** See RFC 822 § APPENDIX D

    DIGIT     =  <any ASCII decimal digit>    ; ( 60- 71, 48.- 57. )
*)
let digit     = ['0' - '9']

(** RFC 822 § 3.3:
                                              ; (  Octal, Decimal. )
    SPACE     =  <ASCII SP, space>            ; (     40,      32. )
    HTAB      =  <ASCII HT, horizontal-tab>   ; (     11,       9. )

    LWSP-char =  SPACE / HTAB                 ; semantics = SPACE

    CR        =  <ASCII CR, carriage return>  ; (     15,      13. )
    LF        =  <ASCII LF, linefeed>         ; (     12,      10. )
    CRLF      =  CR LF
*)
let space     = "\032"
let htab      = "\009"
let lwsp_char = space | htab
let cr        = "\013"
let lf        = "\010"
let crlf      = cr lf

(** See RFC 822 § 3.3 (XXX: Obsolete version):

    specials  = "(" / ")" / "<" / ">" / "@" ; Must be in quoted-
              / "," / ";" / ":" / "\" / <"> ;  string, to use
              / "." / "[" / "]"             ; within a word.
*)
let specials  =
  '(' | ')' | '<' | '>' | '@' |
  ',' | ';' | ':' | '\\' | '"' |
  '.' | '[' | ']'

(** See RFC 822 § APPENDIX D

    atom      = 1*<any CHAR except specials, SPACE and CTLs>
*)
let atom      = ([^ '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\007'
                    '\008' '\009' '\010' '\011' '\012' '\013' '\014' '\015'
                    '\016' '\017' '\018' '\019' '\020' '\021' '\022' '\023'
                    '\024' '\025' '\026' '\027' '\028' '\029' '\030' '\031'
                    ' ' '(' ')' '<' '>' '@' ',' ';' ':' '"' '.' '[' ']'
                    '\\' '\127' ]) +

(** See RFC 822 § 3.3 (XXX: Obsolete version):

                                                    ; (  Octal, Decimal. )
    BS                 = <ASCII BS, backslash>      ; (    134,      92. )
    linear-white-space = 1*([CRLF] LWSP-char)       ; semantics = SPACE
                                                    ; CRLF => folding
    quoted-pair        = BS CHAR                    ; may quote any char
    ctext              = <any CHAR excluding "(",   ; => may be folded
                          ")", BS & CR, & including
                          linear-white-space>
*)
let quoted_pair = '\\' ['\000' - '\127']
let linear_white_space =  (crlf ? lwsp_char) +
let ctext =
  (* XXX: [^ '(' ')' …] | linear_white_space
          or [^ '(' ')' … linear_white_space] *)
  [^ '(' ')' '\\' '\013'] | linear_white_space

(** See RFC 822 § 3.4.5:

    Where permitted (i.e.,  in  words in structured fields)  quoted- strings are
    treated as a  single symbol.  That is,  a quoted-string is  equivalent to an
    atom,  syntactically.  If a  quoted-string is to  be 'folded'  onto multiple
    lines,  then the syntax  for folding must be adhered  to.  (See the 'Lexical
    Analysis of  Messages' section on  'Folding Long Header  Fields' above,  and
    the  section  on  "Case   Independence"  below.)  Therefore,   the  official
    semantics do not  "see" any bare CRLFs that  are in quoted-strings;  however
    particular  parsing programs  may wish  to  note  their  presence.  For such
    programs,  it would be reasonable to interpret a "CRLF LWSP-char" as being a
    CRLF which  is part of the  quoted-string;  i.e.,  the CRLF is  kept and the
    LWSP-char is discarded.  Quoted  CRLFs (i.e.,  a backslash followed  by a CR
    followed by a LF) are also subject to rules of folding,  but the presence of
    the quoting character  (backslash)  explicitly  indicates  that  the CRLF is
    data to the  quoted string.  Stripping off the first  following LWSP-char is
    also appropriate when parsing quoted CRLFs.

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
let qtext =
  linear_white_space | ['\000' - '\033'] | ['\035' - '\091'] | ['\096' - '\127']
(** XXX: ignore or accept 0 .. 32 (controls characters) ? *)
let quoted_string = '"' (qtext | quoted_pair) '"'

(** See RFC 822 § APPENDIX D:

    dtext = <any CHAR excluding "[",              ; => may be folded
             "]", '\', & CR, & including
             linear-white-space>
*)
let dtext = linear_white_space
            | ['\000' - '\012']
            | ['\014' - '\090']
            | ['\094' - '\127']

(** See RFC 822 § 3.3

    word = atom / quoted-string
*)
let word = atom | quoted_string

(** See RFC 822 § 6.1:

    addr-spec      =  local-part "@" domain        ; global address
    local-part     =  word *("." word)             ; uninterpreted
                                                   ; case-preserved
    domain         =  sub-domain *("." sub-domain)
    sub-domain     =  domain-ref / domain-literal
    domain-ref     =  atom                         ; symbolic reference

    See RFC 822 § 3.3 and 3.4.6 and 6.2.3:

    Square  brackets ("["  and "]")  are used  to  indicate  the  presence  of a
    domain-literal,  which  the  appropriate  name-domain  is  to  use directly,
    bypasing normal name-resolution mechanisms.

    Domain-literals which refer  to  domains  within  the  ARPA Internet specify
    32-bit  Internet  addresses,  in four  8-bit  fields  noted  in decimal,  as
    described in Request for Comments #820, "Assigned Numbers." For example:

    [10.0.3.19]

    Note:  THE USE OF DOMAIN-LITERALS  IS STRONGLY DISCOURAGED.  It is permitted
           only as a  means of bypassing temporary  system limitations,  such as
           name tables which are not complete.

    The names  of "top-level" domains,  and the  names of  domains under  in the
    ARPA Internet,  are  registered  with  the  Network Information Center,  SRI
    International, Menlo Park, California.

    domain-literal = "[" *(dtext / quoted-pair) "]"

*)
let domain_ref     = atom
let domain_literal = "[" (dtext | quoted_pair) * "]"
let sub_domain     = domain_ref | domain_literal
let domain         = sub_domain ('.' sub_domain) *

let local_part     = word ('.' word) *
let addr_spec      = local_part '@' domain

(** See RFC 822 § APPENDIX D

    msg-id = "<" addr-spec ">" ; Unique message id
*)
let msg_id         = '<' addr_spec '>'

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

    If a  comment is to  be 'folded' onto  multiple lines,  then the  syntax for
    folding must be adhered to.  (See the 'Lexical Analysis of Messages' section
    on  'Folding  Long   Header  Fields'  above,   and  the   section  on  'Case
    Independence' below.) Note  that  the  official  semantics  therefore do not
    'see' any unquoted CRLFs that are in comments, although particular pars- ing
    programs may wish to note  their presence.  For these programs,  it would be
    reasonable to interpret a  "CRLF LWSP-char" as being a CRLF  that is part of
    the comment;  i.e., the CRLF is kept and the LWSP-char is discarded.  Quoted
    CRLFs (i.e.,  a backslash followed  by a CR followed by a  LF) still must be
    followed by at least one LWSP-char.
*)
rule comment level = parse
  | '('                 { comment (level + 1) lexbuf }
  | ')'
    { if level <= 1 then (assert (level = 1); lexbuf)
      else comment (level - 1) lexbuf }
  | ctext
  | quoted_pair  { comment level lexbuf }
  | _                   { raise Lexical_error }

(** XXX: quoted-string like string from ocaml with respect RFC 822 *)
and  quoted_string buffer = parse
  | qtext as t
    { Buffer.add_string buffer t;
      quoted_string buffer lexbuf }
  | quoted_pair as t
    { Buffer.add_string buffer t;
      quoted_string buffer lexbuf }
  | '"'
    {  Buffer.contents buffer }
  | _ { raise Lexical_error }

and  msg_id = parse
  | msg_id as data { data }
  | _              { raise Lexical_error }
