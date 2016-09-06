(** Module Decoder (Parser)

    Most           of           this            part           is           from
    {{:https://github.com/inhabitedtype/angstrom}angstrom}  project   by  Spiros
    Eliopoulos. But MrMime is not a parser combinator library! And if you want a
    good parser combinator library,  {i angstrom} is certainly the best in OCaml
    (what I know).  However,  {i angstrom} is not  as permissive as we want - we
    need to do  some weird tricks and optimize (with  {i ocamllex} for example)
    some computations.

    We expose this part  if you want to play with some  explicit parsers for the
    email.  And for this way, we provide all combinators but, as we said, MrMime
    is not a parser combinator  library,  this module it's just some convenience
    combinators to deal between the rest of MrMime.
*)

module Input : module type of RingBuffer.Committed
  with type 'a t = 'a RingBuffer.Committed.t

type s = Parser.s =
  | Complete (** If the client has no data after. *)
  | Incomplete (** If the client can gave some data after. *)

val pp : Format.formatter -> s -> unit

type err = Parser.err = ..

type ('a, 'input) state = ('a, 'input) Parser.state =
  | Read of { buffer : 'input Input.t; k : int -> s -> ('a, 'input) state }
    (** The parser requires more input. *)
  | Done of 'a
    (** The parser succeeded. *)
  | Fail of string list * err
    (** The parser failed. *)

type ('a, 'input) k    = 'input Input.t -> s -> 'a
type ('a, 'input) fail = (string list -> err -> ('a, 'input) state, 'input) k
type ('a, 'r, 'input) success = ('a -> ('r, 'input) state, 'input) k

(** A parser for values of type ['a]. *)
type 'a t = 'a Parser.t =
  { f : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k }

(** [return v] creates a parser that will always succeed and return [v]. *)
val return : 'a -> 'a t

(** [fail exn] creates a parser that will always fail with the {!err} [exn]. *)
val fail   : err -> 'a t

(** [p >>= f] creates a parser that will run [p], pass its result to [f], run
    the parser that [f] produces, and return its result.
*)
val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t

(** [p >>| f] creates a parser that will run [p], and if it succeeds with result
    [v], will return [f v].
*)
val (>>|)  : 'a t -> ('a -> 'b) -> 'b t

(** [p <|> q] runs [p] and returns the result if succeeds. If [p] fails, then
    the input will be reset and [q] will run instead.
*)
val (<|>)  : 'a t -> 'a t -> 'a t

(** [f <$> p] is equivalent to [p >>| f]. *)
val (<$>)  : ('a -> 'b) -> 'a t -> 'b t

(** [p <* q] runs [p], then runs [q], discards its result, and returns the
    result of [p].
*)
val (<* )  : 'a t -> 'b t -> 'a t

(** [p *> q] runs [p], discards its result and then runs [q]. *)
val ( *>)  : 'a t -> 'b t -> 'b t

(** [f <*> p] is equivalent to [f >>= fun f -> p >>| f]. *)
val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t

(** [fix f] computes the fixpoint of [f] and runs the resultant parser. The
    argument that [f] receives is the result of [fix f], which [f] must use,
    paradoxically, to define [fix f].

    [fix] is useful when constructing parsers for inductively-defined types such
    as sequences, trees, etc (like [`Multipart] for MrMime).
*)
val fix    : ('a t -> 'a t) -> 'a t

val lift   : ('a -> 'b) -> 'a t -> 'b t
val lift2  : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3  : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** The [liftN] family of functions promote functions to the parser monad. For
    any of theses functions, the following equivalence holds:

    {[liftn f p ... pn = f <$> p1 <*> ... <*> pn]}
*)

(** [run input p] runs [p] on [input]. *)
val run    : 'input Input.t -> 'a t -> ('a, 'input) state

(** [only input p] runs [p] on [input]. This compute consider [input] as already
    [Complete], so this compute never returns {!Read}. 
*)
val only   : 'input Input.t -> 'a t -> ('a, 'input) state

type err += Satisfy
type err += String
type err += Repeat

(** [peek_chr] accepts any char and return  it,  or returns [None] if the end of
    input has been reached.

    This parser does not advance the input. Use it for lookahead.
*)
val peek_chr     : char option t

(** [peek_chr_exn] accepts any  char and returnss it.  If end  of input has been
    reached, it will fail and returns {!End_of_flow}.

    This parser does not advance the input. Use it for lookahead.
*)
val peek_chr_exn : char t

(** [advance n] advances the input of [n] bytes.

    {b NOTE}:  [assert false]  if you want to advance [n]  byte(s) but the input
    has not enough.
*)
val advance      : int -> unit t

(** [satisfy f] accepts any character for which [f] returns [true] and returns
    the accepted character.
*)
val satisfy      : (char -> bool) -> char t

(** [string f s] ensures the input has [String.length s] (in another case,
    the parser fails with {!End_of_flow}) and compare the string [s'] from the
    input and [s] with [f s = f s'] (in another case, the parser fails with
    {!String}). The parser advances the input of [String.length s] byte(s).
*)
val string       : (string -> string) -> string -> string t

(** [store buf  f] stores any  character for which  [f] returns [true]  in [buf]
    only on the continuous buffer  inside the input.  That means,  [store buf f]
    can't consume all character of the input.

    You need to use [peek_chr] to continue or  not to store the data if you want
    to consume all  character  which  respects  the  predicate [f].  This parser
    returns of byte consumed (and it advance the input).
*)
val store        : Buffer.t -> (char -> bool) -> int t

(** [recognize f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.

    This parser does not fail. If [f] returns [false] on the first character, it
    will return the empty string.
*)
val recognize    : (char -> bool) -> string t

(** [char c] accepts [c] and return it. *)
val char         : char -> char t

(** [many p] runs [p] {i zero} or more times and returns a list of results from
    the runs of [p].
*)
val many         : 'a t -> 'a list t

(** [one p] runs [p] {i one} or more times and returns a list of results from
    the runs of [p].
*)
val one          : 'a t -> 'a list t

(** [option v p] runs [p], returning the result of [p] if it succeeds and [v] if
    it fails.
*)
val option       : 'a -> 'a t -> 'a t

(** [take n] accepts exactly [n] character(s) of input and returns them as a
    string.
*)
val take         : int -> string t

(** [list ps] runs each [p] in [ps] in sequence, returning a list of results of
    each [p].
*)
val list         : 'a t list -> 'a list t

(** [count n p] runs [p] [n] times, returning a list of the results. *)
val count        : int -> 'a t -> 'a list t

(** [repeat      a      b      f]       is      a      parser      from      the
    {{:https://tools.ietf.org/html/rfc5234#section-3.6}RFC5234} to recognize any
    character which respect the predicate [f]. The parser expects [a] characters
    and it is limited by [b] length.

    - [repeat (Some 2) (Some 2) is_digit] expects only 2 digit characters.
    - [repeat None (Some 2) is_digit] expects at most 2 digit characters.
    - [repeat (Some 2) None is_digit] expects at least 2 digit chracters.
    - [repeat None None is_digit] is same as [recognize is_digit].

    If the parser can't respect the constraint [a] and [b], it fails with
    {!Repeat}.
*)
val repeat       : int option -> int option -> (char -> bool) -> string t
