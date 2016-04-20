type 'a t = 'a option

val value : 'a t -> default:'a -> 'a
val bind  : ('a -> 'b) -> 'a t -> 'b t
