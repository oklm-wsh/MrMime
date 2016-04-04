{
  exception Lexical_error
}

(* from RFC 822 *)

let space              = "\032"
let htab               = "\009"
let lwsp_char          = space | htab
let cr                 = "\013"
let lf                 = "\010"
let crlf               = cr lf
let linear_white_space = (crlf ? lwsp_char) +
let digit              = ['0' - '9']

(** See RFC 2822 § 2.2.2:

    … the space (SP,  ASCII value 32)  and horizontal tab (HTAB,  ASCII value 9)
    characters (together known as the white space characters, WSP) …

    XXX: Same as RFC 822 § 3.3.
*)
let space     = space
let htab      = htab
(** The previous name of wsp, in RFC 822, is LWSP_char *)
let wsp       = space | htab

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
let tspecials =
  '(' | ')' | '<' | '>' | '@' | ',' | ';' |
  ':' | '"' | '.' | '[' | ']' | '\\'

(** See RFC 2822 § 2.1 or part of RFC 822 § 3.3 *)
let cr        = cr
let lf        = lf
let crlf      = crlf

(** Deliberately chose to include bare CR and LF here, although the RFC suggests
    them to  be included as part  of the  text characters.  The  rationale being
    that,  when parsing e-mail from a  text,  they will probably mean CRLF.  The
    issue should not arise in conforming e-mails.

    XXX: should be just [cr lf] *)
let crlf      = crlf | lf

(** See RFC 2822 § 4.2:

    In the obsolete  syntax,  any amount of folding white  space MAY be inserted
    where the obs-FWS  rule is allowed.  This creates the  possibility of having
    two consecutive "folds" in a line, and therefore the possibility that a line
    which makes  up a folded  header field could  be composed entirely  of white
    space.

    obs-FWS = 1*WSP *(CRLF 1*WSP)
*)
let obs_fws   = wsp + (crlf wsp +) *

(** See RFC 2822 § 4.1:

    The obs-char and obs-qp elements each add ASCII value 0.

    obs-qp   = %d92 (%d0-127)
    obs-char = %d0-9 / %d11 /  ; %d0-127 except CR and
               %d12 / %d14-127 ; LF
*)
let obs_qp    = '\\' ['\000' - '\127']
let obs_char  = ['\000' - '\009'] | '\011' | '\012' | ['\014' - '\127']

(** See RFC 2822 § 4.1:

    Bare CR and bare LF are added to obs-text and obs-utext.

    obs-text  = *LF *CR *(obs-char *LF *CR)
    obs-utext = obs-text
*)
let obs_text  =
  lf * cr * (obs_char lf * cr * ) *
let obs_utext = obs_text

(** Folding White Space, see RFC 2822 § 3.2.3

    FWS = ([*WSP CRLF] 1*WSP) /   ; Folding white space
          obs-FWS

    XXX: [ r ] in this regexp means an optional sequence (see RFC 2234 § 3.8)
*)
let fws       = ((wsp * crlf) ? wsp + ) | obs_fws

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
let no_ws_ctl = ['\001' - '\008'] | '\011' | '\012' | ['\014' - '\031'] | '\127'
let text = ['\001' -'\009'] | '\011' | '\012' | ['\014' - '\127'] | obs_text

(** See RFC 2822 § 3.2.2:

    Some characters are reserved for special interpretation,  such as delimiting
    lexical tokens.  To permit use of these characters as uninterpreted data,  a
    quoting mechanism is provided.

    quoted-pair = (%d92 text) / obs-qp
*)
let quoted_pair = ('\\' text) | obs_qp

(** See RFC 2822 § 3.2.3:

    ctext = NO-WS-CTL /     ; Non white space controls
            %d33-39 /       ; The rest of the US-ASCII
            %d42-91 /       ;  characters not including "(",
            %d93-126        ;  ")", or "\""
*)
let ctext = no_ws_ctl | ['\033' - '\039'] | ['\042' - '\091'] | ['\093' - '\126']

let qtext = no_ws_ctl | '\033' | ['\035' - '\091'] | ['\093' - '\126']
let dtext = no_ws_ctl | ['\033' - '\090'] | ['\094' - '\126']

(** See RFC 2822 § 3.2.4

    atext         = ALPHA / DIGIT / ; Any character except controls,
                    "!" / "#" /     ;  SP, and specials.
                    "$" / "%" /     ;  Used for atoms.
                    "&" / "'" /
                    "*" / "+" /
                    "-" / "/" /
                    "=" / "?" /
                    "^" / "_" /
                    "`" / "{" /
                    "|" / "}" /
                    "~"
    atom          = [CFWS] atext+ [CFWS]
    dot-atom      = [CFWS] dot-atom-text [CFWS]
    dot-atom-text = 1*atext *("." 1*atext)
*)
let atext = ['a' - 'z' 'A' - 'Z' '0' - '9']
  | '!' | '#' | '$' | '%' | '&' | '~' | '*' | '+'
  | '-' | '/' | '=' | '?' | '^' | '_' | '`' | '{'
  | "|" | '}' | '\''

let atom          = (* [CFWS] *) atext + (* [CFWS] *)
let dot_atom_text = atext+ ('.' atext+) *
let dot_atom      = (* [CFWS] *) dot_atom_text (* [CFWS] *)

(** See RFC 2822 § 3.6.4:

    Though  optional,   every   message  SHOULD  have   a  "Message-ID:"  field.
    Furthermore,  reply  messages SHOULD  have "In-Reply-To:"  and "References:"
    fields as appropriate, as described below.

    The "Message-ID:"  field contains a  single unique  message identifier.  The
    "References:"  and "In-Reply-To:"  field each  contain  one  or  more unique
    message identifiers, optionally separated by CFWS.

    The  message identifier  (msg-id) is  similar  in  syntax  to  an angle-addr
    construct without the internal CFWS.

    message-id      =       "Message-ID:" msg-id CRLF
    in-reply-to     =       "In-Reply-To:" 1*msg-id CRLF
    references      =       "References:" 1*msg-id CRLF
    msg-id          =       [CFWS] "<" id-left "@" id-right ">" [CFWS]
    id-left         =       dot-atom-text / no-fold-quote / obs-id-left
    id-right        =       dot-atom-text / no-fold-literal / obs-id-right
    no-fold-quote   =       DQUOTE *(qtext / quoted-pair) DQUOTE
    no-fold-literal =       "[" *(dtext / quoted-pair) "]"
*)

(*
let obs_id_left     = local_part
let obs_id_right    = domain
*)

let no_fold_quote   = '"' (qtext | quoted_pair) * '"'
let no_fold_literal = '[' (dtext | quoted_pair) * ']'
let id_left         = dot_atom_text | no_fold_quote (* | obs_id_left *)
let id_right        = dot_atom_text | no_fold_literal (* | obs_id_right *)
let msg_id          = (* [CFWS] *) '<' id_left '@' id_right '>' (* [CFWS] *)

(** See RFC 2822 § 3.2.3:

    Strings of  characters enclosed  in parentheses  are considered  comments so
    long as they do not appear  within a "quoted-string",  as defined in section
    3.2.5. Comments may nest.

    Comments can't be expressed with regular expressions, as they're nested:

    ccontent = ctext / quoted-pair / comment
    comment  = "(" *([FWS] ccontent) [FWS] ")"
*)
rule comment level = parse
  |  '('                 { comment (level + 1) lexbuf }

  (* XXX: no need to match quoted-pair or ctext because we match all content
     with the default case of this rule.
  |  quoted_pair
  |  ctext       { comment level lexbuf }
  *)

  |  ')'
    { if level <= 1 then (assert (level = 1); lexbuf)
      else comment (level - 1) lexbuf }
  | _                    { comment level lexbuf }

and  msg_id acc = parse
  | '('                      { msg_id acc (comment 1 lexbuf) }
  | fws                      { msg_id acc lexbuf }
  (* XXX: See RFC 2822 § APPENDIX B:
     CFWS within msg-id is not allowed. *)
  | msg_id as data
    { match acc with
      | Some acc -> raise (Invalid_argument "Lexer.msg_id")
      | None -> msg_id (Some data) lexbuf }
  | _
  (* XXX: we lost a character! *)
    { match acc with
      | Some acc -> acc
      | None -> raise (Invalid_argument "Lexer.msg_id") }
