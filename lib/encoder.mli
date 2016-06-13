type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int }

type 'r partial = [> `Partial of (Bytes.t * int * int * (int -> 'r)) ] as 'r

type 'r           k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)     k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

val make : unit -> t
