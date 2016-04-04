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

(** See RFC 2045 § 5.1:

    tspecials :=  "(" / ")" / "<" / ">" / "@" /
                  "," / ";" / ":" / "\" / <">
                  "/" / "[" / "]" / "?" / "="
                  ; Must be in quoted-string,
                  ; to use within parameter values
*)
let tspecials =
  '(' | ')' | '<' | '>' | '@' | ',' | ';' |
  ':' | '=' | '"' | '/' | '[' | ']' | '?' | '\\'

(** See RFC 2045 § 5.1:

    token := 1* <any (US-ASCII) CHAR excepts SPACE, CTLs or tspecials>

    XXX: [^ space ctls tspecials]
*)
let token = [^ ' ' '\000' - '\031' '\127'
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
let period = '.'
(** XXX: [^ Rfc822.space Rfc822_ctls tspecials period] + *)
let ietf_token = [^ ' ' '\000' - '\031' '\127'
                    '(' ')' '<' '>' '@' ',' ';'
                    ':' '=' '"' '/' '[' ']' '?' '\\'
                    '.'] +

(** See RFC 2045 § 5.1:

    x-token := <The two characters "X-" or "x-" followed, with
                no intervening white space, by any token>

    extension-token := ietf-token / x-token
*)
let x_token = ("x-" | "X-") token
let extension_token = x_token | ietf_token

(** See RFC 2045 § 5.1:

    type := discrete-type / composite-type

    discrete-type  := "text" / "image" / "audio" / "video" /
                      "application" / extension-token

    composite-type := "message" / "multipart" /
                      extension-token
*)
let discrete_ty =
  "text" | "image" | "audio" | "video" | "application" |
  extension_token

let composite_ty =
  "message" | "multipart" |
  extension_token

let ty = discrete_ty | composite_ty

(** XXX: must be registered with IANA, see RFC 2048:

    iana-token := <A publicly-defined extension token. Tokens
                   of this form must be registered with IANA
                   as specified in RFC 2048.>
*)
let iana_token = token

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
let subty = extension_token | iana_token

let attribute = token
let value = token
         (* | rfc822_quoted_string

            XXX: quoted_string is useless (it does not concern the
                 regular expr. but also rule) ! It's handled below. *)

let mechanism = "7bit" | "8bit" | "binary" | "quoted-printable" | "base64"
                | x_token | ietf_token

(** See RFC 2045 § 7:

    In constructing a  high-level user agent,  it may be  desirable to allow one
    body  to make  reference to  another.  Accordingly,  bodies may  be labelled
    using the  "Content-ID" header field,  which  is syntactically  identical to
    the "Message-ID" header field:

    id := "Content-ID" ":" msg-id

    Like  the Message-ID  values,  Content-ID values  must  be  generated  to be
    world-unique.

    The Content-ID value  may be used for uniquely  identifying MIME entities in
    several  contexts,   particularly   for  caching  data   referenced  by  the
    message/external-body  mechanism.   Although   the   Content-ID   header  is
    generally optional,  its use is  MANDATORY in implementations which generate
    data of the optional MIME media type "message/external-body".  That is, each
    message/external-body entity must have a  Content-ID field to permit caching
    of such data.

    It is also  worth noting that the Content-ID value  has special semantics in
    the case of the multipart/alternative  media type.  This is explained in the
    section of RFC 2046 dealing with multipart/alternative.
*)

let id = "Content-ID"
(* let msg_id = rfc822_msg_id *)

(** See RFC 822 § 3.3 or RFC 2045 § 5.1:

    In  addition,  comments are  allowed in  accordance with  RFC 822  rules for
    structured header fields. Thus the following two forms

      Content-type: text/plain; charset=us-ascii (Plain text)

      Content-type: text/plain; charset="us-ascii"

    are completely equivalent.
*)
rule content_type = parse
  | '('                { content_type (Rfc822.comment 1 lexbuf) }
  (** XXX: handle of comment (RFC 822). *)
  | '"'
    { Parser.STRING (Rfc822.quoted_string (Buffer.create 16) lexbuf) }
  (** XXX: handle of quoted-string. *)
  | '/'                { Parser.SLASH }
  | ty as t
  | subty as t
  | attribute as t
  | value as t         { Parser.ATOM t }
  | linear_white_space { content_type lexbuf }
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

      TODO:  we can replace  it by the rfc2822_fws but it  may be not compatible
      with RFC 822 so, we will try this.
  *)
  | ';'                { Parser.SEMICOLON }
  | '='                { Parser.EQUAL }
  | eof                { Parser.EOF }
  | _                  { raise Lexical_error }

and  mechanism = parse
  | "7bit"           { `Bit7 }
  | "8bit"           { `Bit8 }
  | "binary"         { `Binary }
  | "quoted-primary" { `Quoted_primary }
  | "base64"         { `Base64 }
  | x_token as t     { `X_token t }
  | ietf_token as t  { `Ietf_token t }
  | _                { raise (Invalid_argument "Lexer.mechanism") }

(*
and msg_id = parse
  | msg_id as msg_id { msg_id }
  | _                        { raise (Invalid_argument "Lexer.msg_id") }
*)

(** See RFC 2045 § 4

    Since it is possible that a  future document might extend the message format
    standard again,  a formal  BNF is given for the  content of the MIME-Version
    field:

    version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT
*)
and  version = parse
  | digit as digit
    { Parser.DIGIT  (int_of_string (String.make 1 digit)) }
  | linear_white_space        { version lexbuf }
  | '.'                       { Parser.DOT }

  (** XXX: See RFC 2045 § 4:

      When checking  MIME-Version values  any RFC 822  comment strings  that are
      present must be ignored.
  *)
  | '('                       { version (Rfc822.comment 1 lexbuf) }
  | eof                       { Parser.EOF }
  | _                         { raise Lexical_error }
