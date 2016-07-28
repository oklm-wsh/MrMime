type 'a t = 'a RingBuffer.Committed.t

type st = [ `ST ]
type bs = [ `BS ]

val write : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
val write_string : 'a t -> string -> int -> int -> unit

val create_bytes : int -> st t
val create_bigstring : int -> bs t

val size : 'a t -> int
val peek : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
val read : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
val read_space : 'a t -> ('a Internal_buffer.t * int * int) option
val write_space : 'a t -> ('a Internal_buffer.t * int * int) option
val transmit : 'a t -> ('a Internal_buffer.t -> int -> int -> int) -> int
val ravailable : 'a t -> int
val wavailable : 'a t -> int
val radvance : 'a t -> int -> unit
val wadvance : 'a t -> int -> unit
val get : 'a t -> char
val pp : Format.formatter -> 'a t -> unit
val proof : 'a t -> 'a Internal_buffer.t
val savailable : 'a t -> int
