open Parser

type err += Nothing_to_do

type phrase =
  [ `Dot
  | `Word of Rfc822.word
  | `Encoded of (string * Rfc2047.raw) ] list
type domain =
  [ `Domain of string list
  | `Literal of Rfc5321.literal_domain ]
type mailbox =
  { name    : phrase option
  ; local   : Rfc822.local
  ; domain  : domain * domain list }
type group =
  { name    : phrase
  ; mailbox : mailbox list }
type address = [ `Group of group | `Mailbox of mailbox ]
type month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec
type day =
  | Mon | Tue | Wed
  | Thu | Fri | Sat
  | Sun
type zone =
  | UT  | GMT
  | EST | EDT
  | CST | CDT
  | MST | MDT
  | PST | PDT
  | Military_zone of char
  | TZ of int
type date =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; zone : zone }
type unstructured =
  [ `Text of string | `CR of int | `LF of int | `CRLF | `WSP
  | `Encoded of (string * Rfc2047.raw) ] list
type phrase_or_msg_id =
  [ `Phrase of phrase | `MsgID of Rfc822.msg_id ]
type resent =
  [ `ResentDate      of date
  | `ResentFrom      of mailbox list
  | `ResentSender    of mailbox
  | `ResentTo        of address list
  | `ResentCc        of address list
  | `ResentBcc       of address list
  | `ResentMessageID of Rfc822.msg_id
  | `ResentReplyTo   of address list ]
type trace =
  [ `Trace of ((Rfc822.local * (domain * domain list)) option
               * ([ `Addr   of Rfc822.local * (domain * domain list)
                  | `Domain of domain
                  | `Word   of Rfc822.word ] list * date option) list) ]
type field =
  [ `Date       of date
  | `From       of mailbox list
  | `Sender     of mailbox
  | `ReplyTo    of address list
  | `To         of address list
  | `Cc         of address list
  | `Bcc        of address list
  | `MessageID  of Rfc822.msg_id
  | `InReplyTo  of phrase_or_msg_id list
  | `References of phrase_or_msg_id list
  | `Subject    of unstructured
  | `Comments   of unstructured
  | `Keywords   of phrase list
  | resent
  | trace
  | `Field      of string * unstructured
  | `Unsafe     of string * unstructured ]
type skip =
  [ `Skip       of string ]

val is_digit         : char -> bool
val is_military_zone : char -> bool
val is_obs_utext     : char -> bool
val is_ftext         : char -> bool

val addr_spec        : (Rfc822.local * domain) t
val obs_phrase       : phrase t
val phrase           : phrase t
val display_name     : phrase t
val obs_domain_list  : domain list t
val obs_route        : domain list t
val obs_angle_addr   : (Rfc822.local * (domain * domain list)) t
val angle_addr       : (Rfc822.local * (domain * domain list)) t
val name_addr        : (phrase option * (Rfc822.local * (domain * domain list))) t
val mailbox          : mailbox t
val obs_mbox_list    : mailbox list t
val obs_group_list   : unit t
val mailbox_list     : mailbox list t
val group_list       : mailbox list t
val group            : group t
val address          : address t
val address_list     : address list t

val obs_hour         : int t
val obs_minute       : int t
val obs_second       : int t
val hour             : int t
val minute           : int t
val second           : int t
val obs_year         : int t
val year             : int t
val obs_day          : int t
val month            : month t
val day_name         : day t
val obs_day_of_week  : day t
val day_of_week      : day t
val date             : (int * month * int) t
val time_of_day      : (int * int * int option) t
val obs_zone         : zone t
val zone             : zone t
val time             : ((int * int * int option) * zone) t
val date_time        : date t

val obs_unstruct     : unstructured t
val unstructured     : unstructured t
val phrase_or_msg_id : phrase_or_msg_id list t
val obs_phrase_list  : phrase list t
val keywords         : phrase list t
val field_name       : string t

val field            : (string -> ([> field ] as 'a) t) -> string -> 'a t
val skip             : string t
val header           : (string -> ([> skip | field ] as 'a) t) -> 'a list t

val decode           : unit t -> unit t -> string t
