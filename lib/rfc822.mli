open Parser

type word   = [ `Atom of string | `String of string ]
type domain = [ `Domain of string list | `Literal of string ]
type local  = word list
type msg_id = (local * domain)

val is_vchar         : char -> bool
val is_ctext         : char -> bool
val is_obs_no_ws_ctl : char -> bool
val is_qtext         : char -> bool
val is_atext         : char -> bool
val is_cr            : char -> bool
val is_lf            : char -> bool
val is_d0            : char -> bool
val is_wsp           : char -> bool
val is_quoted_pair   : char -> bool
val is_dtext         : char -> bool

val quoted_pair      : char t
val wsp              : char t
val crlf             : unit t
val obs_fws          : (bool * bool * bool) t
val fws              : (bool * bool * bool) t
val comment          : unit t
val cfws             : unit t
val qcontent         : string t
val quoted_string    : string t
val atom             : string t
val word             : word t
val obs_local_part   : local t
val dot_atom_text    : string list t
val dot_atom         : string list t
val local_part       : local t
val obs_domain       : string list t
val domain_literal   : string t
val domain           : domain t
val id_left          : local t
val no_fold_literal  : string t
val id_right         : domain t
val msg_id           : msg_id t
