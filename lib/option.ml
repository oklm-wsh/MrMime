type 'a t = 'a option

let value a ~default = match a with
  | Some x -> x
  | _      -> default

let bind f = function
  | Some x -> Some (f x)
  | None   -> None
