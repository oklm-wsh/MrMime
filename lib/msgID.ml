open BasePP

type left  = Rfc5322.left
type right = Rfc5322.right
type t     = { left : left; right : right; }

let p = Format.fprintf

let pp_right fmt = function
  | `Domain l -> p fmt "%a" (pp_list ~sep:"." pp_atom) l
  | `Literal s -> p fmt "[%a]" (pp_string ~in_qs:false ~in_dm:true) s

let pp_left fmt =
  p fmt "%a" (pp_list ~sep:"." pp_word)

let pp fmt { left; right; } =
  p fmt "<%a@%a>" pp_left left pp_right right

let of_lexer (left, right) =
  { left; right; }

let of_string s =
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
  loop @@ BaseLexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n"))

let to_string t =
  let tmp = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer tmp in

  Format.fprintf fmt "%a%!" pp t;
  Buffer.contents tmp

let equal = (=)
