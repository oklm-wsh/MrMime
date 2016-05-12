type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

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

val pp_char  : Format.formatter -> char -> unit
val pp_lst   : ?sep:string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_error : Format.formatter -> error -> unit

type     err = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

exception Error of err

val err                     : error -> t -> err
val err_unexpected_eoi      : t -> err
val err_expected            : char -> t -> err
val err_expected_set        : char list -> t -> err
val err_unexpected          : char -> t -> err
val err_unexpected_str      : string -> t -> err
val err_expected_str        : string -> t -> err
val err_wrong_padding       : t -> err
val err_unexpected_encoding : string -> t -> err
val err_invalid_ipv6        : t -> err
val err_invalid_ipv4        : t -> err
val err_invalid_ipv4v6      : t -> err
val err_invalid_tag         : string -> t -> err
val err_invalid_field       : string -> t -> err
val err_nothing_to_do       : t -> err
val err_invalid_header      : t -> err
val err_unexpected_field    : string -> t -> err
val err_invalid_boundary    : string -> t -> err
val err_expected_boundary   : t -> err

val safe       : ('a -> ([> err ] as 'err)) -> 'a -> 'err
val read_exact : int -> (string -> t -> ([> err | 'ret read] as 'ret)) -> t -> 'ret
val roll_back  : (t -> ([> err | 'ret read ] as 'ret)) -> string -> t -> 'ret
val read_line  : (t -> ([> err | 'ret read ] as 'ret)) -> t -> 'ret

val peek_chr   : t -> char option
val cur_chr    : t -> char
val junk_chr   : t -> unit

val p_chr      : char -> t -> unit
val p_str      : string -> t -> unit
val p_set      : char list -> t -> unit
val p_while    : (char -> bool) -> t -> string
val p_try_rule : ('a -> t -> ([> err | 'c read] as 'c))
                 -> (t -> 'c)
                 -> (t -> ([< `Ok of ('a * t) | err | 'e read > `Error] as 'e))
                 -> t -> 'c
val p_repeat   : ?a:int -> ?b:int -> (char -> bool) -> t -> string
val p_try      : (char -> bool) -> t -> int

val make       : ?len:int -> unit -> t
val of_string  : string -> t
val of_bytes   : Bytes.t -> t
