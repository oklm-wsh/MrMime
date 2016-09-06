(** Module Address *)

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
type word           = [ `Atom of string | `String of string ]
type local          = word list

(** It's             an             {i             encoded-word}            from
    {{:https://tools.ietf.org/html/rfc2047}RFC2047}.  An  {i encoded-word}  is a
    sequence of printable  ASCII characters that begins  with ["=?"],  ends with
    ["?="],  and has two ["?"]s in between.  It specifies a character set and an
    encoding method,  and  also includes  the original  text encoded  as graphic
    ASCII characters, according to the rules for that encoding method.

    MrMime recognizes {i encoded-words} when  they appear in certain protions of
    the message header.  Instead of displaying the {i encoded-word} "as is",  it
    will reverse the encoding and display the original text.

    {b NOTE}: the client need to translate the original text into the designated
    character set (like [utf-8]) - this feature is in {b TODO}.

    @see <https://tools.ietf.org/html/rfc2047> RFC2047
*)
type raw = Rfc2047.raw =
  | QuotedPrintable of string
  | Base64 of MrMime_base64.Decoder.result

type literal_domain = Rfc5321.literal_domain = ..
type literal_domain += IPv4 of Ipaddr.V4.t
type literal_domain += IPv6 of Ipaddr.V6.t
type phrase         =
  [ `Dot
  | `Word of word
  | `Encoded of (string * raw) ] list
type domain         =
  [ `Domain of string list
  | `Literal of literal_domain ]

type mailbox = Rfc5322.mailbox =
  { name    : phrase option
  ; local   : local
  ; domain  : domain * domain list }

type group = Rfc5322.group =
  { name    : phrase
  ; mailbox : mailbox list }

type address = [ `Group of group | `Mailbox of mailbox ]

val pp_word         : Format.formatter -> word -> unit
val pp_domain       : Format.formatter -> domain -> unit
val pp_phrase       : Format.formatter -> phrase -> unit
val pp_local        : Format.formatter -> local -> unit
val pp_mailbox'     : Format.formatter -> (local * (domain * domain list)) -> unit
val pp_mailbox      : Format.formatter -> mailbox -> unit
val pp_group        : Format.formatter -> group -> unit
val pp              : Format.formatter -> address -> unit

module Encoder :
sig
  val w_word        : (word,         'r Encoder.partial) Wrap.k1
  val w_domain      : (domain,       'r Encoder.partial) Wrap.k1
  val w_safe_string : (string,       'r Encoder.partial) Wrap.k1
  val w_raw         : (raw,          'r Encoder.partial) Wrap.k1
  val w_phrase      : (phrase,       'r Encoder.partial) Wrap.k1
  val w_local       : (local,        'r Encoder.partial) Wrap.k1
  val w_mailbox'    : ((local * (domain * domain list)), 'r Encoder.partial) Wrap.k1
  val w_mailbox     : (mailbox,      'r Encoder.partial) Wrap.k1
  val w_group       : (group,        'r Encoder.partial) Wrap.k1
  val w_address     : (address,      'r Encoder.partial) Wrap.k1
  val w_addresses   : (address list, 'r Encoder.partial) Wrap.k1
end

module Decoder :
sig
  val p_address     : address      MrMime_parser.t
  val p_addresses   : address list MrMime_parser.t
  val p_local       : local        MrMime_parser.t
  val p_domain      : domain       MrMime_parser.t
end

val to_string       : address -> string
val of_string       : ?chunk:int -> string -> address option
val of_string_raw   : ?chunk:int -> string -> int -> int -> (address * int) option

val equal           : address -> address -> bool

module List :
sig
  val pp            : Format.formatter -> address list -> unit

  val to_string     : address list -> string
  val of_string     : ?chunk:int -> string -> address list option
  val of_string_raw : ?chunk:int -> string -> int -> int -> (address list * int) option

  val equal         : address list -> address list -> bool
end

module Make :
sig
  type z = Z
  type 'a s = S

  type 'a word
  type ('data, 'peano) llist

  val word  : string -> [ `Atom | `String ] word
  val atom  : string -> [> `Atom ] word

  val e     : ([> `Atom ] word, z) llist

  val ( & ) : 'a word -> ('a word, 'x) llist -> ('a word, 'x s) llist

  val ( @ ) : ([ `Atom | `String ] word, _ s) llist -> ([ `Atom ] word, _ s) llist -> mailbox
end

module Extension :
sig
  val add_literal_domain : string -> literal_domain MrMime_parser.t -> unit
end
