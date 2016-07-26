type version = Rfc2045.version
type field   = [ `MimeVersion of version ]

(* convenience alias *)
module Input = MrMime_input

let pp = Format.fprintf

let pp fmt (a, b) =
  pp fmt "(%d, %d)" a b

let default = (1, 0)

module Encoder =
struct
  open Encoder

  let w_version (a, b) =
    let open Wrap in
    hovbox 0
    $ string (string_of_int a)
    $ close_box
    $ string "."
    $ hovbox 0
    $ string (string_of_int b)
    $ close_box

  let w_crlf k e = string "\r\n" k e

  let w_field = function
    | `MimeVersion v ->
      string "MIME-Version: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_version v $ close_box) (unlift k))))
      $ w_crlf
end

module Decoder =
struct
  let p_version = Rfc2045.version
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

  aux 0 @@ Parser.run i Parser.(Rfc2045.version <* Rfc822.crlf)

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

  aux off @@ Parser.run i Rfc2045.version
