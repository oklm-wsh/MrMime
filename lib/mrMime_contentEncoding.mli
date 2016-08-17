(** Module Encoding *)

(** Kind of encoding.  Many media types  which could be usefully transported via
    email are represented,  in their {i natural} format, as [`Bit8] character or
    [`Binary]  data.   Such  data  cannot  be  transmitted  over  some  transfer
    protocols. For example, {{:https://tools.ietf.org/html/rfc821}RFC821} (SMTP)
    restricts mail messages to [`Bit7] US-ASCII data whitch lines no longer than
    1000 characters including any trailing [CRLF] line separator.

    Three transformations are currently defined: identity, the
    ["quoted-printable"] encoding, and the ["base64"] encoding. The domains are
    ["binary"], ["8bit"] and ["7bit"].

    The ["Content-Transfer-Encoding"] values ["7bit"],  ["8bit"], and ["binary"]
    all  mean that  the identity  (i.e.  No)  encoding  transformation  has been
    performed.  As such,  they serve  simply as indicators of the  domain of the
    body data,  and provide  useful information about the sort  of encoding that
    might be needed for transmission in a given transport system.

    The ["quoted-printable"] and ["base64"] encodings transform their input from
    an arbitrary domain into material in the ["7bit"] range, thus making it safe
    to carry over restricted  transports.  You can see {!MrMime.QuotedPrintable}
    for  the   ["quoted-printable"]  decoding   and  {!MrMime.Base64}   for  the
    ["base64"] decoding.
*)
type mechanism =
  [ `Base64
  | `Binary
  | `Bit7
  | `Bit8
  | `Ietf_token of string
  | `QuotedPrintable
  | `X_token of string ]

(** It is necessary,  therefore,  to define  a standard [mechanism] for encoding
    such data into  a [`Bit7] short line format.  Proper  labelling of unencoded
    material in  less restrictive formats  for direct use  over less restrictive
    transports is also desirable. This module specifies that such encodings will
    be indicated by a new ["Content-Transfer-Encoding"] header field.
*)
type field     = [ `ContentEncoding of mechanism ]

(** [pp mechanism] prints an human readable representation of [mechanism]. *)
val pp            : Format.formatter -> mechanism -> unit

(** An encoding type of  [`Bit7] requires that the body is  already in a [`Bit7]
    mail-ready  representation.   This   is  the  default  value   --  that  is,
    ["Content-Transfer-Encoding:      7BIT"]      is     assumed      if     the
    ["Content-Transfer-Encoding"] header field is not present.
*)
val default       : mechanism

module Encoder :
sig
  val w_encoding  : (mechanism, 'r Encoder.partial) Encoder.k1
  val w_field     : (field, 'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  (** See RFC2045 § {{:https://tools.ietf.org/html/rfc2045#section-6.1}6.1}:

      {[
      encoding := "Content-Transfer-Encoding" ":" mechanism

      mechanism := "7bit" / "8bit" / "binary" /
                   "quoted-printable" / "base64" /
                   ietf-token / x-toke
      ]}

      These values are not case sensitive  -- [Base64] and [BASE64] and [bAsE64]
      are all equivalent.
  *)
  val p_encoding  : mechanism MrMime_parser.t
end

(** [of_string           ~chunk:1024           buf]           parses          an
    {{:https://tools.ietf.org/html/rfc2045#section-6}RFC2045}       {!mechanism}
    starting at [0] in [buf].

    This function allocates a internal buffer with [chunk] size (default to
    [1024]).
*)
val of_string     : ?chunk:int -> string -> mechanism option

(** [of_string_raw     ~chunk:1024      buff     off     len]      parses     an
    {{:https://tools.ietf.org/html/rfc2045#section-6}RFC2045}       {!mechanism}
    starting at [off] in [buf] to a tuple [(mechanism, count)] with:
    - [mechanism] the {!mechanism}
    -  [count]  the  number  of  bytes  read  starting  at  [off]  to  parse the
    [mechanism].

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024]).
*)
val of_string_raw : ?chunk:int -> string -> int -> int -> (mechanism * int) option
