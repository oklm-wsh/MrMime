open Parser

type err += Invalid_token of string

type discrete =
  [ `Text
  | `Image
  | `Audio
  | `Video
  | `Application ]
type composite =
  [ `Message
  | `Multipart ]
type extension =
  [ `Ietf_token of string
  | `X_token    of string ]

type ty = [ discrete | composite | extension ]

type subty =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token    of string ]

type mechanism =
  [ `Bit7
  | `Bit8
  | `Binary
  | `QuotedPrintable
  | `Base64
  | `Ietf_token of string
  | `X_token    of string ]

type value = [ `String of string | `Token of string ]

type content =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

type version = (int * int)

type field =
  [ `ContentType        of content
  | `ContentEncoding    of mechanism
  | `ContentID          of Rfc822.msg_id
  | `ContentDescription of Rfc5322.unstructured
  | `Content            of (string * Rfc5322.unstructured) ]
type unsafe =
  [ `Unsafe             of (string * Rfc5322.unstructured) ]
type skip =
  [ `Skip               of string ]

type field_version =
  [ `MimeVersion of version ]

val is_tspecials         : char -> bool
val is_ctl               : char -> bool
val is_space             : char -> bool
val is_token             : char -> bool
val is_digit             : char -> bool

val token                : string t
val attribute            : string t
val ietf_token           : string t
val iana_token           : string t
val x_token              : string t
val extension_token      : extension t

val composite_ty         : [ composite | extension ] t
val ty                   : ty t
val subty                : ty -> (ty * subty) t
val value                : value t
val parameter            : (string * value) t

val content              : content t
val version              : version t
val mechanism            : mechanism t
val encoding             : mechanism t
val id                   : Rfc822.msg_id t

val part_field           : (string -> ([> field | unsafe ] as 'a) t) -> (string -> 'a t) -> string -> 'a t
val message_field        : (string -> ([> field | field_version ] as 'a) t) -> (string -> 'a t) -> string -> 'a t
val mime_message_headers : (string -> ([> field | field_version ] as 'a) t) -> (string -> 'a t) -> 'a list t
val mime_part_headers    : (string -> ([> skip | unsafe | field ] as 'a) t) -> 'a list t
