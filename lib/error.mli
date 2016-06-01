type error =
  [ `Unexpected_eoi
  | `Expected_char       of char
  | `Expected_set        of char list
  | `Unexpected_char     of char
  | `Unexpected_str      of string
  | `Expected_str        of string
  | `Wrong_padding
  | `Unexpected_encoding of string
  | `Invalid_ipv6
  | `Invalid_ipv4
  | `Invalid_ipv4v6
  | `Invalid_tag         of string
  | `Invalid_field       of string
  | `Nothing_to_do
  | `Invalid_header
  | `Unexpected_field    of string
  | `Invalid_boundary    of string
  | `Expected_boundary ]

type     err = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

exception Error of err

val err                     : error -> Decoder.t -> err
val err_unexpected_eoi      : Decoder.t -> err
val err_expected            : char -> Decoder.t -> err
val err_expected_set        : char list -> Decoder.t -> err
val err_unexpected          : char -> Decoder.t -> err
val err_unexpected_str      : string -> Decoder.t -> err
val err_expected_str        : string -> Decoder.t -> err
val err_wrong_padding       : Decoder.t -> err
val err_unexpected_encoding : string -> Decoder.t -> err
val err_invalid_ipv6        : Decoder.t -> err
val err_invalid_ipv4        : Decoder.t -> err
val err_invalid_ipv4v6      : Decoder.t -> err
val err_invalid_tag         : string -> Decoder.t -> err
val err_invalid_field       : string -> Decoder.t -> err
val err_nothing_to_do       : Decoder.t -> err
val err_invalid_header      : Decoder.t -> err
val err_unexpected_field    : string -> Decoder.t -> err
val err_invalid_boundary    : string -> Decoder.t -> err
val err_expected_boundary   : Decoder.t -> err

val pp : Format.formatter -> error -> unit
