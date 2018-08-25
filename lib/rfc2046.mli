open Parser

val is_bcharsnospace         : char -> bool
val is_bchars                : char -> bool

val make_dash_boundary       : string -> string
val make_delimiter           : string -> string
val make_close_delimiter     : string -> string

type ('field, 'a) octet = (Content.t -> ([ Rfc5322.field | Rfc2045.field | Rfc5322.skip ] as 'field) list -> 'a t)

val dash_boundary            : string -> unit t
val discard_to_dash_boundary : string -> unit t
val transport_padding        : unit t
val text                     : unit t
val discard_text             : unit t
val delimiter                : string -> unit t
val close_delimiter          : string -> unit t
val discard_to_delimiter     : string -> unit t
val body_part                : ('field, 'a) octet -> (Content.t * 'field list * 'a option) t
val encapsulation            : string -> ('field, 'a) octet -> (Content.t * 'field list * 'a option) t
val preamble                 : string -> unit t
val epilogue                 : string option -> unit t
val multipart_body           : string option -> string -> ('field, 'a) octet -> (Content.t * 'field list * 'a option) list t
