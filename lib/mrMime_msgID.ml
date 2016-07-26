type local  = Rfc822.local
type word   = Rfc822.word
type domain = Rfc822.domain
type msg_id = Rfc822.msg_id

(* convenience alias *)
module Address = MrMime_address
module Input   = MrMime_input

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_word = Address.pp_word
let pp_local = Address.pp_local

let pp_domain fmt (x : domain) = match x with
  | `Domain lst ->
    pp fmt "@[<hov>%a@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt ".") Format.pp_print_string) lst
  | `Literal s -> pp fmt "[@[<hov>%s@]]" s

let pp fmt (local, domain) =
  pp fmt "{@[<hov>local = %a;@ domain = %a@]}"
    pp_local local
    pp_domain domain

module Encoder =
struct
  open Encoder
  open Wrap

  let w_left = Address.Encoder.w_local

  let w_right = function
    | `Domain lst ->
      let rec aux = function
        | [] -> noop
        | [ x ] -> string x
        | x :: r -> string x $ hovbox 0 $ string "." $ aux r $ close_box
      in hovbox 0 $ aux lst $ close_box
    | `Literal lit ->
      hovbox 0
      $ string "["
      $ string lit
      $ string "]"
      $ close_box

  let w_msg_id (local, domain) =
    hovbox 0
    $ char '<'
    $ hovbox 1
    $ w_left local
    $ close_box
    $ char '@'
    $ hovbox 1
    $ w_right domain
    $ close_box
    $ char '>'
    $ close_box
end

module Decoder =
struct
  let p_msg_id = Rfc822.msg_id
end

let of_string ?(chunk = 1024) s =
  let s' = s ^ "\r\n" in
  let l = String.length s' in
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (l - consumed) in
      Input.write_string buffer s' consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  aux 0 @@ Parser.run i Parser.(Rfc822.msg_id <* Rfc822.crlf)

let of_string_raw ?(chunk = 1024) s off len =
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (len - (consumed - off)) in
      Input.write_string buffer s consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some (v, consumed - off)
  in

  aux off @@ Parser.run i Rfc822.msg_id
