type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int }

type 'r partial = [> `Partial of (Bytes.t * int * int * (int -> 'r)) ] as 'r

type 'r           k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)     k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

val make : unit -> t

val flush  : ('r partial) k0
val string : (string, 'r partial) k1
val char   : (char, 'r partial) k1
val noop   : ('a -> 'b) -> 'a -> 'b
val sp     : ('a, unit, string) format -> 'a

val ( $ )  : (('a -> 'b) -> 'a -> 'b) -> (('a -> 'b) -> 'a -> 'b) -> (('a -> 'b) -> 'a -> 'b)
val ( & )  : ('r partial) k0 -> ('r partial) k0 -> ('r partial) k0
