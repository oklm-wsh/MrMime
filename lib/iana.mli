module Set : (module type of Set.Make(String))
module Map : (module type of Map.Make(String))

type t = Set.t Map.t

val database : t
