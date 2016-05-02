module Map = Map.Make(String)
module Set = Set.Make(String)

type mtype = Set.t Map.t
type tag = Set.t

let mtype = Map.empty
let mtype = [%mtype mtype "mtype.xml"]

let tag = Set.empty
let tag = [%tag tag "tag.xml"]
