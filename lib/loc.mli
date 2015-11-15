type t

(** Constructors *)
val make : Lexing.position -> Lexing.position -> t
val unknown : t

(** Computation *)
val join : t -> t -> t

(** Conversion *)
val of_lexbuf : Lexing.lexbuf -> t

(** Pretty-printing *)
val pp : Format.formatter -> t -> unit
val pp_of_file : ?hidden:char list -> Format.formatter -> t -> unit
