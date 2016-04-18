val is_obs_no_ws_ctl  : char -> bool
val is_d0             : char -> bool
val is_lf             : char -> bool
val is_cr             : char -> bool
val is_vchar          : char -> bool
val is_wsp            : char -> bool
val is_ctext          : char -> bool
val is_digit          : char -> bool
val is_alpha          : char -> bool
val is_atext          : char -> bool
val is_specials       : char -> bool
val is_qtext          : char -> bool
val is_dquote         : char -> bool
val is_obs_utext      : char -> bool

val is_valid_atext    : string -> bool

val s_wsp             : char list

val p_ctext           : Lexer.t -> string
val p_atext           : Lexer.t -> string
val p_qtext           : Lexer.t -> string

type atom   = [ `Atom of string ]
type word   = [ atom | `String of string ]
type phrase = [ word | `Dot | `FWS ] list

val p_comment         : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_ccontent        : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_group_list  : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_unstruct    : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_unstructured    : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

val p_quoted_pair     : (char        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_fws             : (bool        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_cfws            : (bool        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_atom            : (string      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_dot_atom        : (atom list   -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_quoted_string   : (string      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_word            : (word        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_phrase          : (phrase      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_hour        : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_minute      : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_second      : (int -> bool -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_hour            : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_minute          : (int -> bool -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_second          : (int -> bool -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_year        : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_year            : bool -> (int -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_day         : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_day             : (int         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

type month =
  [ `Jan | `Feb | `Mar | `Apr
  | `May | `Jun | `Jul | `Aug
  | `Sep | `Oct | `Nov | `Dec ]

type day =
  [ `Mon | `Tue | `Wed | `Thu
  | `Fri | `Sat | `Sun ]

type tz =
  [ `TZ of int
  | `UT
  | `GMT | `EST | `EDT | `CST | `CDT
  | `MST | `MDT | `PST | `PDT
  | `Military_zone of char ]

type date = int * month * int
type time = int * int * int option

val p_month           : (month                         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_day_of_week     : (day                           -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_date            : (date                          -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_time_of_day     : (bool -> time                  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_zone        : (tz                            -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_zone            : bool -> (tz                    -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_time            : (time * tz                     -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_date_time       : (day option * date * time * tz -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

type domain =
  [ `Literal of string
  | `Domain of atom list ]

type local   = word list
type person  = phrase option * (local * domain list)
type group   = phrase * person list
type address = [ `Group of group | `Person of person ]

val p_dtext           : (string              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_domain      : (atom list           -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_local_part  : (local               -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_domain_literal  : (string              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_domain          : (domain              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_local_part      : (local               -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_domain_list : (domain list         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_route       : (domain list         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_angle_addr  : (local * domain list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_angle_addr      : (local * domain list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_display_name    : (phrase              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_name_addr       : (person              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_mailbox         : (person              -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_mbox_list   : (person list         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_mailbox_list    : (person list         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_group_list      : (person list         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_group           : (group               -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_address         : (address             -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_addr_list   : (address list        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_address_list    : (address list        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
