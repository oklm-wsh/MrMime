(** Module QuotedPrintable encoding *)

open Parser

module Decoder :
sig
  (** [is_hex chr] returns [true] iff  the character is a hexadecimal character.
      The standard defines an hexadecimal character like that:

      {[
      HEX   = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
      DIGIT = "0" - "9"
      ]}

      MrMime accepts ["a" - "f"] characters too.
  *)
  val is_hex          : char -> bool

  (** Corresponding            to            the            rule            from
      {{:https://tools.ietf.org/html/rfc2045#section-6.7}RFC2045}    about   the
      hexadecimal value:

      {[
      hex-octet := "=" 2(DIGIT / "A" / "B" / "C" / "D" / "E" / "F")
                   ; Octet must be used for characters > 127, =,
                   ; SPACEs or TABs at the ends of lines, and is
                   ; recommended for any character not listed in
                   ; RFC2049 as "mail-safe".
      ]}

      As we see above,  MrMime accepts ["a" - "f"] characters too.  This decoder
      returns the hexadecimal value as [char].
  *)
  val hex             : char t

  (** [decode boundary  rollback] decodes a  QuotedPrintable input and  stops at
      the [boundary] and, iff he decoder recognizes the boundary, it rollbacks -
      that means the input contains the boundary although it recognized.
  *)
  val decode          : unit t -> unit t -> string t

  (** [inline  ()] decodes  an {i  inline  encoded  string}.  It  stops  when it
      recognizes the ["?"] character and it does not consume him.
  *)
  val inline          : unit -> string t
end

module Encoder :
sig
  val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
  val w_encode        : string -> ('r Encoder.partial) Encoder.k0
end

module Input : module type of RingBuffer.Committed
  with type 'a t = 'a RingBuffer.Committed.t

(* The type for QuotedPrintable decoder. *)
type 'a decoder

type decoding =
  [ `Continue
  | `Error of err
  | `Dirty of char
  | `End of string
  | `String of string ]

(** [decoder  (boundary,  rollback) input]  is  a  QuotedPrintable  decoder that
    inputs from  [input].  [boundary] specifies  the limit  of the  decoding and
    [rollback] allows the decoder to keep the boundary inside the [input].
*)
val decoder       : (unit Parser.t * unit Parser.t) -> 'a Input.t -> 'a decoder

(** [decoder_src decoder] is [decoder]'s input source. *)
val decoder_src   : 'a decoder -> 'a Input.t

(** [decode decoder] is:
    - [`Continue] if [decoder] awaits for more input. The client muse use {!src}
    to provide it.
    -  [`Dirty  chr] if  [decoder]  recognizes  an  unsafe  character (according
    {{:https://tools.ietf.org/html/rfc2045#section-6.7}RFC2045}:

    {[
    safe-char := <any octet with decimal value of 33 through
                 60 inclusive, and 62 through 126>
                 ; Characters not listed as "mail-safe" in
                 ; RFC2049 are also not recommended.
    ]}

    In this case,  the  character is already skipped.  The client  can choose to
    keep this character in its computation or not.

    - [`Error exn] if the [decoder] as a parsing error.
    - [`String s] if a chunk [s] was decoder.
    - [`End s] if  the last chunk [s] was decoded -  when the decoder arises one
    time  this  value,  it  returns only  this  value  after  the  client recall
    [decode].
*)
val decode        : 'a decoder -> decoding

(** [src decoder buf  off  len]  provides  [decoder]  with  [len] bytes to read,
    starting at [off] in [buf].  This bbyte  range is read by calls to {!decode}
    until [`Continue] is returned.  To signal the end of input call the function
    with [len = 0].
*)
val src           : 'a decoder -> bytes -> int -> int -> unit
