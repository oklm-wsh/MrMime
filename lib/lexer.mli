type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

type error =
  [ `Unexpected_eoi
  | `Expected_char   of char
  | `Expected_set    of char list
  | `Unexpected_char of char
  | `Unexpected_str  of string ]

val pp_char  : Format.formatter -> char -> unit
val pp_lst   : ?sep:string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_error : Format.formatter -> error -> unit

type e = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

exception Error of e

val err                : error -> t -> e
val err_unexpected_eoi : t -> e
val err_expected       : char -> t -> e
val err_expected_set   : char list -> t -> e
val err_unexpected     : char -> t -> e
val err_unexpected_str : string -> t -> e

val safe       : ('a -> ([> e ] as 'err)) -> 'a -> 'err
val read_exact : int -> (string -> t -> ([> e | 'ret read] as 'ret)) -> t -> 'ret
val roll_back  : (t -> ([> e | 'ret read ] as 'ret)) -> string -> t -> 'ret
val read_line  : (t -> ([> e | 'ret read ] as 'ret)) -> t -> 'ret

val peek_chr   : t -> char option
val cur_chr    : t -> char
val junk_chr   : t -> unit

val p_chr      : char -> t -> unit
val p_set      : char list -> t -> unit
val p_while    : (char -> bool) -> t -> string

val make       : ?len:int -> unit -> t
val of_string  : string -> t
val of_bytes   : Bytes.t -> t
