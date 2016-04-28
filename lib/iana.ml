module Map = Map.Make(String)
module Set = Set.Make(String)

type t = Set.t Map.t

let database = Map.empty
let database = [%iana database "iana.xml"]
