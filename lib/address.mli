type local          = Rfc822.local
type word           = Rfc822.word
type raw            = Rfc2047.raw = QuotedPrintable of string | Base64 of Base64.result
type literal_domain = Rfc5321.literal_domain = ..
type phrase         = Rfc5322.phrase
type domain         = Rfc5322.domain

type mailbox = Rfc5322.mailbox =
  { name    : phrase option
  ; local   : local
  ; domain  : domain * domain list }

type group = Rfc5322.group =
  { name    : phrase
  ; mailbox : mailbox list }

type address = Rfc5322.address

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
  val w_word        : (word, 'r Encoder.partial) Wrap.k1
  val w_domain      : (domain, 'r Encoder.partial) Wrap.k1
  val w_safe_string : (string, 'r Encoder.partial) Wrap.k1
  val w_raw         : (raw, 'r Encoder.partial) Wrap.k1
  val w_phrase      : (phrase, 'r Encoder.partial) Wrap.k1
  val w_local       : (local, 'r Encoder.partial) Wrap.k1
  val w_mailbox'    : ((local * (domain * domain list)), 'r Encoder.partial) Wrap.k1
  val w_mailbox     : (mailbox, 'r Encoder.partial) Wrap.k1
  val w_group       : (group, 'r Encoder.partial) Wrap.k1
  val w_address     : (address, 'r Encoder.partial) Wrap.k1
  val w_addresses   : (address list, 'r Encoder.partial) Wrap.k1
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
