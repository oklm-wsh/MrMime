(** Module Content-Type

    The purpose of the ["Content-Type"] field  is to describe the data contained
    in  the  body  fully  enough that  the  receiving  user  agent  can  pick an
    appropriate agent or mechanism to present the date to the user, or otherwise
    deal with  the data in  an appropriate manner.  The  value in this  field is
    called a media type.

    The ["Content-Type"]  header field specifies the  nature of the  data in the
    body of  an entity  by giving  media type  and subtype  identifiers,  and by
    providing  auxiliary information  that may  be  required  for  certain media
    types.  After the media type and subtype names,  the remainder of the header
    field is  simply a  set of  parameters,  specified in  a ["attribute=value"]
    notation. The ordering of parameters is not significant.
*)

(** In general, the top-level media type is used to declare the general type of
    data.

    An   initial  set   of  seven   top*level   media   types   is   defined  in
    {{:https://tools.ietf.org/html/rfc2046}RFC2046}.    Five   of    these   are
    {!const:MrMime_message.Discrete} types whose content  is essentially opaque as far
    as    MIME   processing    is    concerned.    The    remaining    two   are
    {!const:MrMime_message.Composite} types whose contents require additional handling
    by MIME processors.

    If another top-level type is to be used for any reason, it must be given a
    name starting with ["X-"] to indicate its non-standard status and to avoid a
    potential conflict with a future officiel name.
*)
type ty      =
  [ `Application
  | `Audio
  | `Ietf_token of string
  | `Image
  | `Message
  | `Multipart
  | `Text
  | `Video
  | `X_token of string ]

(** While the subtype specifies a specific format for that type of data. Thus, a
    media type of ["image/xyz"] is enough to  tell a user agent that the data is
    an image,  even  if the user  agent has no  knowledge of the  specific image
    format ["xyz"].  Such information be used, for example, to decide whether or
    not to  show a  user the  raw data from  an unrecognized  sbtype --  such an
    action might  be reasonable for  unrecognized subtype of  text,  but not for
    unrecognized subtypes of image or audio.

    For this reason, registered subtypes of text,  image, audio and video should
    not contain  embedded information tht  is really of  a different type.  Such
    compound  formats   should  be  represented   using  the   ["multipart"]  or
    ["application"] types.

    MrMime recognize 3 subtypes:
    -                    Subtype                     from                    the
    {{:http://www.iana.org/assignments/media-types/media-types.xhtml}IANA database}:
    [`Iana_token]
    - Valid subtype but not recognized by the IANA database: [`Ietf_token]
    - Like {!ty}, a non-standard subtype starting with ["X-"]: [`X_token]
*)
type subty   =
  [ `Ietf_token of string
  | `Iana_token of string
  | `X_token of string ]

(** Parameters  are  modifiers  of  the  media  subtype,  and  as  such  do  not
    fundamentally  affter the  nature of  the  content.  The  set  of meaningful
    parameters depends on the media type and subtype, However, a given top-level
    media type may define parameters which are applicable to any subtype of that
    type.

    Parameters may be required by their defining content type of subtype or they
    may be optional.  MrMime  does  not  check  any  assumption about parameters
    except for the [`Multipart] where he expects the["boundary"] parameter - but
    MrMime does not processing (not yet!) the ["charset"] parameter for example.

    Note that the value of parameter can be a quoted string ([`String]). In this
    case, the value does not include the quotes. That is, the quotation marks in
    a quoted-string are not a part of the value of the parameter, but are merely
    used to delimit that parameter value. In other case, the value is [`Token].
*)
type value   = [ `String of string | `Token of string ]

(** The field of ["Content-Type"]. *)
type field   = [ `ContentType of Rfc2045.content ]

(** A convenience record to deal with the ["Content-Type"] which contains:
    - the media type [ty]
    - the subtype [subty]
    - parameters: a associative list of [(attribute, value)]
*)
type content = Rfc2045.content =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

(** [pp_ty ty] prints an human readable representation of {!ty}. *)
val pp_ty         : Format.formatter -> ty -> unit

(** [pp_subty subty] prints an human readable representation of {!subty}. *)
val pp_subty      : Format.formatter -> subty -> unit

(** [pp_value value] prints an human readable representation of {!value}. *)
val pp_value      : Format.formatter -> value -> unit

(** [pp_value (attribute,  value)]  prints an  human readable  representation of
    [parameter].
*)
val pp_parameter  : Format.formatter -> (string * value) -> unit

(** [pp content] prints an human readable representation of {!content}. *)
val pp            : Format.formatter -> content -> unit

(** Default  {{:https://tools.ietf.org/html/rfc822}RFC822}  messages  without  a
    MIME ["Content-Type"] header are taken by  this protocol to be plain text in
    the US-ASCII character set, which can be excplicitly specified as:

    {[
    Content-Type: text/plain; charset=use-ascii
    ]}

    This is assumed i no ["ContehjntType"] header field is specified. It is also
    recommend  that  this  dfault  be   assumed  when  a  syntactically  invalid
    ["Content-Type"]  header  field  is  encountered.   In  the  presence  of  a
    {!MrMime_mimeVersion.field}   header  field   and   the   absence   of  any
    ["Content-Type"] header field,  a receiving User  Agent can also assume that
    plain US-ASCII text was the  sender's intent.  Plain US-ASCII text may still
    be  assummed in  the absence  of a  ["MIME-Version"] or  the presence  of an
    syntactically  invalid  ["Content-Type"]  header  field,  but  the  sender's
    initent might have been otherwise.
*)
val default       : content

module Encoder :
sig
  val w_type      : (ty,             'r Encoder.partial) Wrap.k1
  val w_subtype   : (subty,          'r Encoder.partial) Wrap.k1
  val w_value     : (value,          'r Encoder.partial) Wrap.k1
  val w_parameter : (string * value, 'r Encoder.partial) Wrap.k1
  val w_content   : (content,        'r Encoder.partial) Wrap.k1
  val w_field     : (field,          'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  (** See RFC2045 § {{:https://tools.ietf.org/html/rfc2045#section-5.1}5.1}:

      {[
      content := "Content-Type" ":" type "/" subtype
                 *(";" parameter)
                 ; Matching of media type and subtype
                 ; is ALWAYS case-insensitive.

      type := discrete-type / composite-type

      discrete-type := "text" / "image" / "audio" / "video" /
                       "application" / extension-token

      composite-type := "message" / "multipart" / extension-token

      extension-token := ietf-token / x-token

      ietf-token := <An extension token defined by a
                     standards-track RFC and registered
                     with IANA.>

      x-token := <The two characters "X-" or "x-" followed, with
                  no intervening white space, by any token>

      subtype := extension-token / iana-token

      iana-token := <A publicly-defined extension token. Tokens
                     of this form must be registered with IANA
                     as specified in RFC 2048.>

      parameter := attribute "=" value

      attribute := token
                   ; Matching of attributes
                   ; is ALWAYS case-insensitive.

      value := token / quoted-string

      token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                  or tspecials>

      tspecials :=  "(" / ")" / "<" / ">" / "@" /
                    "," / ";" / ":" / "\" / <">
                    "/" / "[" / "]" / "?" / "="
                    ; Must be in quoted-string,
                    ; to use within parameter values
      ]}
  *)
  val p_content   : content Parser.t
end

(** [of_string           ~chunk:1024           buf]           parses          an
    {{:https://tools.ietf.org/html/rfc2045#section-5.1}RFC2045}       {!content}
    starting at [0] in [buf]:

    This function  allocates an  internal buffer with  [chunk] size  (default to
    [1024])
*)
val of_string     : ?chunk:int -> string -> content option

(** [of_string_raw     ~chunk:1024      buff     off     len]      parses     an
    {{:https://tools.ietf.org/html/rfc2045#section-5.1}RFC2045}       {!content}
    starting at [off] in [buf] to a tuple [(content, count)]i with:

    - [content] the {!content}
    - [count] the number of byte read starting at [off] to parse the [content].

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024]).
*)
val of_string_raw : ?chunk:int -> string -> int -> int -> (content * int) option
