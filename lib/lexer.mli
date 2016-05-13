type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

val make       : ?len:int -> unit -> t
val of_string  : string -> t
val of_bytes   : Bytes.t -> t
