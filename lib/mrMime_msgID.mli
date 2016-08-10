(** Message ID encoding and decoding *)

(** [word] is a combinations of [atoms] and/or [quoted-strings]:

    {[
    word         =   atom / quoted-string
    ]}

    -  [atom]  is  interpreted  as  a  single  unit,  comprising  the  string of
    characters that make it  up.  Semantically,  the optional comments and [FWS]
    surrounding the rest of the characters are not part of the atom; the atom is
    only the run of [atext] characters in an atom.

    {[
    atext        =   ALPHA / DIGIT /    ; Printable US-ASCII
                     "!" / "#" /        ;  characters not including
                     "$" / "%" /        ;  specials.  Used for atoms.
                     "&" / "'" /
                     "*" / "+" /
                     "-" / "/" /
                     "=" / "?" /
                     "^" / "_" /
                     "`" / "{" /
                     "|" / "}" /
                     "~"

    atom         =   [CFWS] 1*atext [CFWS]
    ]}

    - Strings of characters that include  characters other than those allowed in
    atoms can be represented in a quoted string format, where the characters are
    surrounded by quote characters.

    {[
    qtext           =   %d33 /             ; Printable US-ASCII
                        %d35-91 /          ;  characters not including
                        %d93-126 /         ;  "\\" or the quote character
                        obs-qtext
    obs-qp          =   "\\" (%d0 / obs-NO-WS-CTL / LF / CR)
    obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                        %d11 /             ;  characters that do not
                        %d12 /             ;  include the carriage
                        %d14-31 /          ;  return, line feed, and
                        %d127              ;  white space characters

    quoted-pair     =   ("\\" (VCHAR / WSP)) / obs-qp
    qcontent        =   qtext / quoted-pair

    quoted-string   =   [CFWS]
                        DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                        [CFWS]
    ]}

    A  [quoted-string]  is  treated  as  a  unit.  That  is,  [quoted-string] is
    identical to  atom,  semantically.  Since  a  [quoted-string]  is allowed to
    contain [FWS],  fold  is  permitted.  Also  note  that  since quoted-pair is
    allowed in a [quoted-string],  the quote and backslash characters may appear
    in  a [quoted-string]  so long  as they  appear as  a [quoted-pair].  MrMime
    interprets directly a [quoted-pair] to its value.

    Semantically,  neither the  optional [CFWS] outside of  the quote characters
    nor the  quote characters themselves  are part of  the [quoted-string];  the
    [quoted-string] is  what is contained  beteen the two  quote characters.  As
    stated  ealier,  the ["\\"]  in any  [quoted-pair]  and  the  [CRLF]  in any
    [FWS]/[CFWS]  that appears  within the  [quoted-string] are  semantically {i
    invisible} and therefore not part of the [quoted-string] either.


    @see <https://tools.ietf.org/html/rfc5322#section-3.2.1> RFC5322 § Quoted
    Characters
    @see <https://tools.ietf.org/html/rfc5322#section-3.2.3> RFC5322 § Atom
    @see <https://tools.ietf.org/html/rfc5322#section-3.2.4> RFC5322 § Quoted
    Strings
    @see <https://tools.ietf.org/html/rfc5322#section-3.2.5> RFC5322 §
    Miscellaneous Tokens
*)
type word   = [ `Atom of string | `String of string ]

(** Left-hand side of the ["@"]. *)
type local  = word list

(** Right-hand side of the ["@"]. *)
type domain = [ `Domain of string list | `Literal of string ]

(** The  message  identifier  [msg-id]  syntax  is  a  limited  version  of  the
    [addr-spec] construct  enclosed in the angle  bracket characters,  ["<"] and
    [">"]. Unlike [æddr-spec], this syntax only permits the [dot-atom-text] form
    on  the left-hand  side of  the ["@"]  and  does  not  have  internal [CFWS]
    anywhere in the message identifier.

    @see <https://tools.ietf.org/html/rfc5322#section-3.6.4> RFC5322 §
    Identification Fields
*)
type msg_id = (local * domain)

(** [pp_local  fmt  local]  prints  an  human  readable  representation  of  the
    left-hand side of the ["@"].
*)
val pp_local      : Format.formatter -> local -> unit

(** [pp_domain  fmt  local]  prints  an  human  readable  representation  of the
    right-hand side of the ["@"].
*)
val pp_domain     : Format.formatter -> domain -> unit

(** [pp fmt msg_id] prints an human readable representation of [msg_id]. *)
val pp            : Format.formatter -> msg_id -> unit

module Encoder :
sig
  val w_left      : (local, 'r Encoder.partial) Wrap.k1
  val w_right     : (domain, 'r Encoder.partial) Wrap.k1
  val w_msg_id    : (msg_id, 'r Encoder.partial) Wrap.k1
end

module Decoder :
sig
  (** See RFC5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}3.6.4}
      and RFC822 {{:https://tools.ietf.org/html/rfc822#section-4.1}4.1}:

      {[
      dtext           =   %d33-90 /          ; Printable US-ASCII
                          %d94-126 /         ;  characters not including
                          obs-dtext          ;  "[", "]", or "\\"
      obs-dtext       =   obs-NO-WS-CTL / quoted-pair

      msg-id          =   [CFWS] "<" id-left "@" id-right ">" [CFWS]
      id-left         =   dot-atom-text / obs-id-left
      id-right        =   dot-atom-text / no-fold-literal / obs-id-right

      no-fold-literal =   "[" *dtext "]"
      dot-atom-text   =   1*atext *("." 1*atext)

      local-part      =   dot-atom / quoted-string / obs-local-part

      domain          =   dot-atom / domain-literal / obs-domain
      domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]

      obs-id-left     =   local-part
      obs-id-right    =   domain
      obs-local-part  =   word *("." word)
      obs-domain      =   atom *("." atom)
      ]}

      See {!word} for [atext], [obs-NO-WS-CTL], [quoted-pair], [quoted-string],
      [atom] and [word].
  *)
  val p_msg_id    : msg_id MrMime_parser.t
end

(** [of_string           ~chunk:1024           buf]           parses          an
    {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322}   message   ID
    starting at [0] in [buf].

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024]).
*)
val of_string     : ?chunk:int -> string -> msg_id option

(** [of_string_raw      ~chunk:1024     buf      off     len]      parses     an
    {{:https://tools.ietf.org/html/rfc5322#section-3.6.4}RFC5322}   message   ID
    starting at [off] in [buf] to a tuple [(msg_id, count)] with;

    - [msg_id] the message ID
    - [count] the number of bytes read starting at [off] to parse the date.

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024]).
*)
val of_string_raw : ?chunk:int -> string -> int -> int -> (msg_id * int) option
