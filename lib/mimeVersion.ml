type t = (int * int)

type field = [ `MimeVersion of (int * int) ]

let field_of_lexer x = x

let make a b = (a, b)

let default = (1, 0)

let equal (a, b) (x, y) =
  a = x && b = y

module D =
struct
  let of_lexer v p state = p v state
  let of_lexer' x = x
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_version (a, b) =
      w_hovbox 1
      $ w_string (string_of_int a)
      $ w_close_box
      $ w_char '.'
      $ w_hovbox 1
      $ w_string (string_of_int b)
      $ w_close_box

    let w_crlf k e = w "\r\n" k e

    let w_field = function
      | `MimeVersion x ->
        w "MIME-Version: "
        $ Wrap.lift
        $ Wrap.(fun k -> (w_hovbox (String.length "MIME-Version: ")
                          $ w_version x
                          $ w_close_box) (unlift k))
        $ w_crlf
  end

  let w = Internal.w_field
end

let pp fmt (a, b) = Format.fprintf fmt "%d.%d" a b
