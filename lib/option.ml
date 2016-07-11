let value ~default = function
  | Some v -> v
  | None -> default

let is_some = function
  | Some v -> true
  | None -> false
