type day   = Rfc5322.day =
  | Mon | Tue | Wed
  | Thu | Fri | Sat
  | Sun
type month = Rfc5322.month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec
type zone  = Rfc5322.zone =
  | UT  | GMT
  | EST | EDT
  | CST | CDT
  | MST | MDT
  | PST | PDT
  | Military_zone of char
  | TZ of int

type date = Rfc5322.date =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; zone : zone }

val pp_zone       : Format.formatter -> zone -> unit
val pp_month      : Format.formatter -> month -> unit
val pp_day        : Format.formatter -> day -> unit
val pp            : Format.formatter -> date -> unit

module Encoder :
sig
  val w_day       : (day, 'r Encoder.partial) Wrap.k1
  val w_date      : (date, 'r Encoder.partial) Wrap.k1
  val w_time      : ((int * int * int option), 'r Encoder.partial) Wrap.k1
  val w_zone      : (zone, 'r Encoder.partial) Wrap.k1
  val w_date      : (date, 'r Encoder.partial) Wrap.k1
end

val to_string     : date -> string
val of_string     : ?chunk:int -> string -> date option
val of_string_raw : ?chunk:int -> string -> int -> int -> (date * int) option

val equal         : date -> date -> bool
