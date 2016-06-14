type word     = Rfc5322.word
type domain   = Rfc5322.domain
type local    = Rfc5322.local
type phrase   = Rfc5322.phrase
type mailbox
type person
type t

module D :
sig
  val of_lexer         : Rfc5322.address -> t
  val person_of_lexer  : Rfc5322.person -> person
  val domain_of_lexer  : Rfc5322.domain -> domain
  val mailbox_of_lexer : Rfc5322.mailbox -> mailbox
  val of_decoder       : Decoder.t -> t
end

module E :
sig
  val to_buffer     : t -> Encoder.t -> Buffer.t
  val w             : (t      , 'r Encoder.partial) Wrap.k1
  val w_word        : (word   , 'r Encoder.partial) Wrap.k1
  val w_local       : (local  , 'r Encoder.partial) Wrap.k1
  val w_domain      : (domain , 'r Encoder.partial) Wrap.k1
  val w_phrase      : (phrase , 'r Encoder.partial) Wrap.k1
  val w_person      : (person , 'r Encoder.partial) Wrap.k1
  val w_mailbox     : (mailbox, 'r Encoder.partial) Wrap.k1
  val w_safe_string : (string , 'r Encoder.partial) Wrap.k1
end

val of_string : string -> t
val to_string : t -> string

val equal      : t -> t -> bool
val pp         : Format.formatter -> t -> unit
val pp_person  : Format.formatter -> person -> unit
val pp_phrase  : Format.formatter -> phrase -> unit
val pp_mailbox : Format.formatter -> mailbox -> unit
val pp_word    : Format.formatter -> word -> unit
val pp_domain  : Format.formatter -> domain -> unit

module List :
sig
  module D :
  sig
    val of_lexer   : Rfc5322.address list -> t list
    val of_decoder : Decoder.t -> t list
  end

  module E :
  sig
    val to_buffer  : t list -> Encoder.t -> Buffer.t
    val w          : (t list, 'r Encoder.partial) Wrap.k1
  end


  val of_string : string -> t list
  val to_string : t list -> string

  val equal     : t list -> t list -> bool
  val pp        : Format.formatter -> t list -> unit
end
