type t = (int * int)

type field = [ `MimeVersion of (int * int) ]

let field_of_lexer = function
  | `MimeVersion x -> `MimeVersion x

let make a b = (a, b)

let default = (1, 0)

let equal (a, b) (x, y) =
  a = x && b = y

let of_lexer = function `MimeVersion v -> v

let pp fmt (a, b) =
  Format.fprintf fmt "%d.%d" a b
