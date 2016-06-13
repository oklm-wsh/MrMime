type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

type 'r read = [> `Read of (Bytes.t * int * int * (int -> 'r)) ] as 'r

type 'r k0           = (t -> 'r) -> t -> 'r
type ('a, 'r) k1     = ('a -> t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = ('a -> 'b -> t -> 'r) -> t -> 'r

val make       : ?len:int -> unit -> t
val of_string  : string -> t
val of_bytes   : Bytes.t -> t
