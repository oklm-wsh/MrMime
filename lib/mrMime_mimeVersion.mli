(** Module MIME-Version *)

(** The version of the Internet message body format standard in use. *)
type version = (int * int)

(** We define a new header field, ["MIME-Version"], which is to be used to
    declare the version of the Internet message body format standard in use.
*)
type field   = [ `MimeVersion of version ]

(** [pp version] prints an human readable representation of [version]. *)
val pp            : Format.formatter -> version -> unit

(** Default value of version. *)
val default       : version

module Encoder :
sig
  val w_version   : (version, 'r Encoder.partial) Wrap.k1
  val w_field     : (field, 'r Encoder.partial) Encoder.k1
end

module Decoder :
sig
  (** See RFC2045 § {{:https://tools.ietf.org/html/rfc2045#section-4}4}:

     {[
     version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT
     ]}
  *)
  val p_version   : version MrMime_parser.t
end

(** [of_string           ~chunk:1024           buf]           parses          an
    {{:https://tools.ietf.org/html/rfc2045#section-4}RFC2045}         {!version}
    starting at [0] in [buf].

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024]).
*)
val of_string     : ?chunk:int -> string -> version option

(** [of_string_raw     ~chunk:1024      buff     off     len]      parses     an
    {{:https://tools.ietf.org/html/rfc2045#section-4}RFC2045}         {!version}
    starting at [off] in [buf] to a tuple [(version, count)] with:

    - [version] the {!version}
    - [count] the number of bytes read starting at [off] to parse the version.

    This  function allocates  a internal  buffer with  [chunk] size  (default to
    [1024])
*)
val of_string_raw : ?chunk:int -> string -> int -> int -> (version * int) option
