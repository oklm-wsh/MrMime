module Input : module type of RingBuffer.Committed
  with type 'a t = 'a RingBuffer.Committed.t

module type S =
sig
  type s =
    | Complete
    | Incomplete

  val pp : Format.formatter -> s -> unit

  type err = ..

  type ('a, 'input) state =
    | Read of { buffer : 'input Input.t; k : int -> s -> ('a, 'input) state }
    | Done of 'a
    | Fail of string list * err

  type ('a, 'input) k    = 'input Input.t -> s -> 'a
  type ('a, 'input) fail = (string list -> err -> ('a, 'input) state, 'input) k
  type ('a, 'r, 'input) success = ('a -> ('r, 'input) state, 'input) k

  type 'a t =
    { f : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k }

  val return : 'a -> 'a t
  val fail   : err -> 'a t

  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|)  : 'a t -> ('a -> 'b) -> 'b t
  val (<|>)  : 'a t -> 'a t -> 'a t
  val (<$>)  : ('a -> 'b) -> 'a t -> 'b t
  val (<* )  : 'a t -> 'b t -> 'a t
  val ( *>)  : 'a t -> 'b t -> 'b t
  val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t
  val (<?>)  : 'a t -> string -> 'a t
  val fix    : ('a t -> 'a t) -> 'a t
  val lift   : ('a -> 'b) -> 'a t -> 'b t
  val lift2  : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3  : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val run    : 'input Input.t -> 'a t -> ('a, 'input) state
  val only   : 'input Input.t -> 'a t -> ('a, 'input) state
end

include S

module type I =
sig
  type err += End_of_flow

  val prompt  : 'input Input.t -> ('input Input.t -> s -> ('a, 'input) state) -> ('input Input.t -> s -> ('a, 'input) state) -> ('a, 'input) state
  val expect  : unit t
  val require : int -> 'input Input.t -> s -> ('a, 'input) fail -> (unit, 'a, 'input) success -> ('a, 'input) state
  val ensure  : int -> bytes t
end

module type C =
sig
  type err += Satisfy
  type err += String
  type err += Repeat

  val peek_chr     : char option t
  val peek_chr_exn : char t
  val advance      : int -> unit t
  val satisfy      : (char -> bool) -> char t
  val print        : string -> unit t
  val string       : (string -> string) -> string -> bytes t
  val store        : Buffer.t -> (char -> bool) -> int t
  val recognize    : (char -> bool) -> string t
  val char         : char -> char t
  val many         : 'a t -> 'a list t
  val one          : 'a t -> 'a list t
  val option       : 'a -> 'a t -> 'a t
  val take         : int -> bytes t
  val list         : 'a t list -> 'a list t
  val count        : int -> 'a t -> 'a list t
  val repeat'      : Buffer.t -> int option -> int option -> (char -> bool) -> int t
  val repeat       : int option -> int option -> (char -> bool) -> string t
end

module IO          : I
module Convenience : C
