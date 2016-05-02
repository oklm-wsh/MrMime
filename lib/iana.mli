module Set : (module type of Set.Make(String))
module Map : (module type of Map.Make(String))

type mtype = Set.t Map.t
type tag = Set.t

val mtype : mtype
val tag : tag
