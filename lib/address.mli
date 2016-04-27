type domain   = [ Rfc5322.domain | LiteralDomain.t ]
and  local    = Rfc5322.local
and  encoding = Rfc2047.encoding = QuotedPrintable | Base64
and  phrase   = Rfc5322.phrase
and  mailbox  =
  { local   : local
  ; domain  : domain * domain list }
and  person   =
  { name    : Rfc5322.phrase option
  ; mailbox : mailbox }
and  group    =
  { name    : Rfc5322.phrase
  ; persons : person list }
and  t        = [ `Group of group | `Person of person ]

val of_string : ?relax:bool -> string -> t
val to_string : t -> string
val of_lexer  : ?relax:bool -> Rfc5322.address -> t

val pp        : Format.formatter -> t -> unit
val equal     : t -> t -> bool

val person_of_lexer  : ?relax:bool -> Rfc5322.person -> person
val pp_person        : Format.formatter -> person -> unit

val domain_of_lexer  : ?relax:bool -> Rfc5322.domain -> domain
val pp_domain        : Format.formatter -> domain -> unit

val mailbox_of_lexer : ?relax:bool -> Rfc5322.mailbox -> mailbox
val pp_mailbox       : Format.formatter -> mailbox -> unit

module List :
sig
  type nonrec t = t list

  val of_string : ?relax:bool -> string -> t
  val to_string : t -> string
  val of_lexer  : ?relax:bool -> Rfc5322.address list -> t

  val pp        : Format.formatter -> t -> unit
  val equal     : t -> t -> bool
end
