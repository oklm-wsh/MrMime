(** Module Input *)

(** A ringbuffer. *)
type 'a t = 'a RingBuffer.Committed.t

(** A proof to use a [string] internally *)
type st = Internal_buffer.st = St
(** A proof to use a [(char, int8_unsigned_elt, c_layout) Bigarray.Array1.t] *)
type bs = Internal_buffer.bs = Bs

(** [write input buf off len] writes an internal buffer (with the same proof
    ['a] - see {!st} or {!bs}) starting at [off] to [len] inside the input.
*)
val write : 'a t -> 'a Internal_buffer.t -> int -> int -> unit

(** [write_string input buf off len] writes a [string] starting at [off] to
    [len] inside the input. *)
val write_string : 'a t -> string -> int -> int -> unit

(** [create_bytes size] returns a fresh input of length [size] with the proof {!st}.
    The sequence is uninitialiazed and contains arbitrary bytes.
*)
val create_bytes : int -> st t

(** [create_bigstring size] returns a fresh input of length [size] with the
    proof {!bs}. The sequence is unintialiazed and contains arbitrary bytes.
*)
val create_bigstring : int -> bs t

(** [size input] returns the length of the given input. *)
val size : 'a t -> int

(** [peek input buf off len] same as {!read} but does not advance the read
    pointer.
*)
val peek : 'a t -> 'a Internal_buffer.t -> int -> int -> unit

(** [read input buf off len] read the data inside the input and advance the read
    pointer.
*)
val read : 'a t -> 'a Internal_buffer.t -> int -> int -> unit

(** [read_space input] returns a continuous part of the internal buffer with the
    offset and the length of bytes available to read. If {!ravailable}
    returns [0], [read_space] returns [None].

    The continuous part of the internal buffer is not necessarily a buffer which
    contains all bytes available to read (it's a part).
*)
val read_space : 'a t -> ('a Internal_buffer.t * int * int) option

(** [write_space input] returns a continuous part of the internal buffer with
    the offset and the length of bytes available to write. If
    {!wavailable} returns [0], [write_space] returns [None].

    The continuous part of the internal buffer is not necessarily a buffer which
    contains all bytes available to write (it's a part).
*)
val write_space : 'a t -> ('a Internal_buffer.t * int * int) option

(** [transmit input f] same as [Option.map f (read_space input)]. *)
val transmit : 'a t -> ('a Internal_buffer.t -> int -> int -> int) -> int

(** [ravailable input] returns available bytes to read. *)
val ravailable : 'a t -> int

(** [wavailable input] returns available bytes to write. *)
val wavailable : 'a t -> int

(** [radvance input n] drops [n] bytes. *)
val radvance : 'a t -> int -> unit

(** [wadvance input n] advances the write pointer. *)
val wadvance : 'a t -> int -> unit

(** [get input] gets the first character at the read pointer. *)
val get : 'a t -> char

(** [pp input] prints an human readable representation of [input]. *)
val pp : Format.formatter -> 'a t -> unit

(** [proof input] gets the internal buffer with the proof. *)
val proof : 'a t -> 'a Internal_buffer.t

(** [savailable input] returns available bytes to read or, if the input is
    committed, returns available bytes from the commit to the write pointer.
*)
val savailable : 'a t -> int
