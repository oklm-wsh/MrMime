type atom     = [ `Atom of string ]
type domain   = [ `Domain of atom list | `Literal of string | LiteralDomain.t ]
and  local    = [ atom | `String of string ] list
and  encoding = Rfc2047.encoding = QuotedPrintable | Base64
and  phrase   = [ atom | `Dot | `Encoded of (string * encoding * string) | `String of string | `WSP | `FWS | `CR of int | `LF of int ] list
and  mailbox  =
  { local   : local
  ; domain  : domain * domain list }
and  person   =
  { name    : phrase option
  ; mailbox : mailbox }
and  group    =
  { name    : phrase
  ; persons : person list }
and  t        = [ `Group of group | `Person of person ]

val of_string : string -> t
val to_string : t -> string
val of_lexer  : Rfc5322.address -> t

val pp        : Format.formatter -> t -> unit
val equal     : t -> t -> bool

val person_of_lexer  : Rfc5322.person -> person
val pp_person        : Format.formatter -> person -> unit

val domain_of_lexer  : Rfc5322.domain -> domain
val pp_domain        : Format.formatter -> domain -> unit

val mailbox_of_lexer : Rfc5322.mailbox -> mailbox
val pp_mailbox       : Format.formatter -> mailbox -> unit

module List :
sig
  type nonrec t = t list

  val of_string : string -> t
  val to_string : t -> string
  val of_lexer  : Rfc5322.address list -> t

  val pp        : Format.formatter -> t -> unit
  val equal     : t -> t -> bool
end
