type t = (int * int)

let make a b = (a, b)

let default = (1, 0)

let equal (a, b) (x, y) =
  a = x && b = y

let of_lexer x = x

let pp fmt (a, b) =
  Format.fprintf fmt "%d.%d" a b
