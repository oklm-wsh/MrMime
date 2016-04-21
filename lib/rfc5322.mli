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
val is_ftext          : char -> bool
val is_text           : char -> bool

val is_valid_atext    : string -> bool

val s_wsp             : char list

val p_ctext           : Lexer.t -> string
val p_atext           : Lexer.t -> string
val p_qtext           : Lexer.t -> string
val p_field_name      : Lexer.t -> string

type atom   = [ `Atom of string ]
type word   = [ atom | `String of string ]
type phrase = [ word | `Dot | `FWS ] list

val p_comment         : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_ccontent        : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_group_list  : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_unstruct    : (string      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_unstructured    : (string      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_crlf            : (Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

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

type date      = int * month * int
type time      = int * int * int option
type date_time = day option * date * time * tz

val p_month           : (month        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_day_of_week     : (day          -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_date            : (date         -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_time_of_day     : (bool -> time -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_zone        : (tz           -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_zone            : bool -> (tz   -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_time            : (time * tz    -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_date_time       : (date_time    -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

type domain =
  [ `Literal of string
  | `Domain of atom list ]

type local   = word list
type mailbox = local * domain list
type person  = phrase option * mailbox
type group   = phrase * person list
type address = [ `Group of group | `Person of person ]

val p_dtext           : (string       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_domain      : (atom list    -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_local_part  : (local        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_domain_literal  : (string       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_domain          : (domain       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_local_part      : (local        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_domain_list : (domain list  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_route       : (domain list  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_angle_addr  : (mailbox      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_angle_addr      : (mailbox      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_display_name    : (phrase       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_name_addr       : (person       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_mailbox         : (person       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_mbox_list   : (person list  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_mailbox_list    : (person list  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_group_list      : (person list  -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_group           : (group        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_address         : (address      -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_obs_addr_list   : (address list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_address_list    : (address list -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

type left   = local
type right  = domain
type msg_id = left * right

val p_msg_id          : (msg_id       -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret

type received =
  [ `Domain of domain
  | `Mailbox of mailbox
  | `Word of word ]

type field =
  [ `From            of person list
  | `Date            of date_time
  | `Sender          of person
  | `ReplyTo         of address list
  | `To              of address list
  | `Cc              of address list
  | `Bcc             of address list
  | `Subject         of string
  | `Comments        of string
  | `Keywords        of phrase list
  | `MessageID       of msg_id
  | `InReplyTo       of [`Phrase of phrase | `MsgID of msg_id] list
  | `References      of [`Phrase of phrase | `MsgID of msg_id] list
  | `ResentDate      of date_time
  | `ResentFrom      of person list
  | `ResentSender    of person
  | `ResentTo        of address list
  | `ResentCc        of address list
  | `ResentBcc       of address list
  | `ResentMessageID of msg_id
  | `Received        of received list * date_time option
  | `ReturnPath      of mailbox option
  | `Field           of string * string ]

val p_field           : (field        -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
val p_header          : (field list   -> Lexer.t -> ([> Lexer.e | 'ret Lexer.read ] as 'ret)) -> Lexer.t -> 'ret
