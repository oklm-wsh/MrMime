type left  = Rfc5322.left
type right = Rfc5322.right
type t     = { left : left; right : right; }

let pp = Format.fprintf

let pp_left fmt = function
  | `String s -> pp fmt "%S" s
  | `Atom s -> pp fmt "%s" s

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_right fmt = function
  | `Domain lst -> pp fmt "[@[<hov>%a@]]" (pp_lst ~sep:(fun fmt () -> pp fmt ".") (fun fmt -> function `Atom s -> pp fmt "%s" s)) lst
  | `Literal lst -> pp fmt "[@[<hov>%a@]]" (pp_lst ~sep:(fun fmt () -> pp fmt "@ ") (fun fmt s -> pp fmt "%s" s)) lst

let pp fmt { left; right } =
  pp fmt "{ @[<hov>%a;@ %a@] }" (pp_lst ~sep:(fun fmt () -> pp fmt ".") pp_left) left pp_right right

module D =
struct
  let of_lexer (left, right) =
    { left; right; }

  open BaseDecoder

  let of_decoder state =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Error.pp exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Address.of_string: unterminated string")
      | `Ok data -> of_lexer data
    in

    let rule = Rfc5322.p_msg_id (fun data state -> `Ok data) in
    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_left = Address.E.w_local
    let w_right = Address.E.w_domain

    let w_msg_id { left; right; } =
      w_hovbox 1
      $ w_char '<'
      $ w_hovbox 1
      $ w_left left
      $ w_close_box
      $ w_char '@'
      $ w_hovbox 1
      $ w_right (right :> Address.domain)
      $ w_close_box
      $ w_char '>'
      $ w_close_box
  end

  let w = Internal.w_msg_id

  let to_buffer t state =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> buf
    in

    let rule =
      let open BaseEncoder in
      let ok = flush (fun _ -> `Ok) in
      Wrap.lift Wrap.(Internal.w_msg_id t (unlift ok))
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string s)
let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

let equal = (=)
