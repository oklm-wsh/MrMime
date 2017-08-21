let value ~default = function
  | Some v -> v
  | None -> default

let is_some = function
  | Some _ -> true
  | None -> false
