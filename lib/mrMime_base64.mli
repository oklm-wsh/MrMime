(** Base64 encoding and decoding *)

open Parser

type result =
  [ `Dirty of string (** In Base64 data, character [chr] does not respecting the
                         predicate [is_b64] probably indicates a transmission
                         error. In this case, the decoder returns [`Dirty raw]. *)
  | `Clean of string (** A clean raw data decoded. *)
  | `Wrong_padding   (** see {!Convenience.Wrong_padding} *)
  ]

(** [is_b64 chr] returns [true] iff the character is a Base64 character. *)
val is_b64          : char -> bool

(** [decode boundary rollback] decodes a Base64 input and stops at the
    [boundary] and, iff the decoder recognizes the boundary, it rollbacks - that
    means the input contains the boundary although it recognized. *)
val decode          : unit t -> unit t -> result t

(** [inline ()] decodes an {i inline encoded string}. It stops when it
    recognizes the ["?"] character and it does not consume him. *)
val inline          : unit -> result t

module Convenience :
sig
  module Input : module type of RingBuffer.Committed
    with type 'a t = 'a RingBuffer.Committed.t

  (** Special processing is performed if fewer than 24 bits are available at the
      end of the data. A full encoding quantum is always completed at the end of
      a body.  When fewer  than 24 inputs bits are available  in an input group,
      zero bits  are added (on  the right) to  form an integral  number of 6-bit
      groups.  Padding at the  end  of  the  data  is  performed using the ["="]
      character.

      Since all Base64 input is an integral number of octets, only the following
      cases can arise:

      - the final quantum of encoding input  is an integral multiple of 24 bits;
      here the final  unit of encoded output  will be an integral  multiple of 4
      characters with no ["="] padding.
      - the final quantum of encoding input is exactly 8 bits;  here,  the final
      unit of encoded  output  will  be  two  characters  followed  by two ["="]
      padding characters.
      - the final quantum of encoding  input is exactly 16 bits;  here the final
      unit of encoded  output  will  be  three  characters  followed by on ["="]
      padding   character.
      - in another case, the decoder returns [Wrong_padding].
  *)
  type err += Wrong_padding

  (** The type for Base64 decoder. *)
  type 'a decoder

  type decoding =
    [ `Continue
    | `Error of err
    | `Dirty of string
    | `End of string
    | `String of string ]

  (** [decoder (boundary,  rollback) input] is a Base64 decoder that inputs from
      [input].  [boundary] specifies  the limit of  the decoding  and [rollback]
      allows the decoder to keep the boundary inside the [input]. *)
  val decoder       : (unit Parser.t * unit Parser.t) -> 'a Input.t -> 'a decoder

  (** [decoder_src decoder] is [decoder]'s input source. *)
  val decoder_src   : 'a decoder -> 'a Input.t

  (** [decode decoder] is:
      - [`Continue]  if [decoder]  awaits for more  input.  The client  must use
      {!src} to provide it.
      - [`Dirty s] if [decoder] has a padding error or if the character is not a
      valid Base64 character (see {!is_b64}).  If  the client is interested in a
      best-effort decoding it can still continue to decode after an dirty chunk.
      - [`Error exn] if [decoder] has  a padding error ([Wrong_padding] error) -
      in this case, the client doest not trust the content - or a parsing error.
      - [`String s] if a chunk [s] was decoded.
      - [`End s] if the last chunk [s] was decoded - when the decoder arises one
      time this value,  it  returns  only  this  value  after  the client recall
      [decode].
  *)
  val decode        : 'a decoder -> decoding

  (** [src decoder  buf off len] provides  [decoder] with  [len] bytes  to read,
      starting at [off] in [buf].  This byte range is read by calls to {!decode}
      until  [`Continue] is  returned.  To signal  the  end  of  input  call the
      function with [len = 0].
  *)
  val src           : 'a decoder -> string -> int -> int -> unit
end

val w_inline_encode : string -> ('r Encoder.partial) Encoder.k0
val w_encode        : string -> ('r Encoder.partial) Encoder.k0
