type ty      = Rfc2045.ty
type subty   = Rfc2045.subty
type value   = Rfc2045.value
type field   = [ `ContentType of Rfc2045.content ]

type content = Rfc2045.content =
  { ty         : Rfc2045.ty
  ; subty      : Rfc2045.subty
  ; parameters : (string * Rfc2045.value) list }

let pp = Format.fprintf

let pp_ty fmt = function
  | `Text         -> pp fmt "text"
  | `Image        -> pp fmt "image"
  | `Audio        -> pp fmt "audio"
  | `Video        -> pp fmt "video"
  | `Application  -> pp fmt "application"
  | `Message      -> pp fmt "message"
  | `Multipart    -> pp fmt "multipart"
  | `Ietf_token s -> pp fmt "ietf:\"%s\"" s
  | `X_token s    -> pp fmt "x:\"%s\"" s

let pp_subty fmt = function
  | `Ietf_token s -> pp fmt "ietf:\"%s\"" s
  | `X_token s    -> pp fmt "x:\"%s\"" s
  | `Iana_token s -> pp fmt "iana:%s" s

let pp_value fmt = function
  | `String s -> pp fmt "%S" s
  | `Token s  -> pp fmt "\"%s\"" s

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_parameter fmt (key, value) =
  pp fmt "%s = %a" key pp_value value

let pp fmt { ty; subty; parameters; } =
  pp fmt "{@[<hov>type = %a/%a;@ parameters = [@[<hov>%a@]]@]}"
    pp_ty ty
    pp_subty subty
    (pp_lst ~sep:(fun fmt () -> pp fmt ";@ ") pp_parameter) parameters

let default =
  { ty = `Text
  ; subty = `Iana_token "plain"
  ; parameters = ["charset", `Token "us-ascii"] }

module Encoder =
struct
  open Encoder

  let w_type = function
    | `Text         -> Wrap.string "text"
    | `Image        -> Wrap.string "image"
    | `Audio        -> Wrap.string "audio"
    | `Video        -> Wrap.string "video"
    | `Application  -> Wrap.string "application"
    | `Message      -> Wrap.string "message"
    | `Multipart    -> Wrap.string "multipart"
    | `Ietf_token s | `X_token s -> Wrap.string s

  let w_subtype = function
    | `X_token s
    | `Iana_token s
    | `Ietf_token s -> Wrap.string s

  let w_value = function
    | `String v -> Wrap.char '"' $ Address.Encoder.w_safe_string v $ Wrap.char '"'
    | `Token s -> Wrap.string s

  let w_parameter (key, value) =
    let open Wrap in
    hovbox 0
    $ string key
    $ close_box
    $ char '='
    $ hovbox 0
    $ w_value value
    $ close_box

  let w_content { ty; subty; parameters; } =
    let open Wrap in
    let w_lst w_sep w_data l =
      let rec aux = function
        | [] -> noop
        | x :: r -> w_sep $ hovbox 0 $ w_data x $ close_box $ aux r
      in aux l
    in
    hovbox 0
    $ w_type ty
    $ close_box
    $ char '/'
    $ hovbox 0
    $ w_subtype subty
    $ close_box
    $ hovbox 0
    $ w_lst (char ';' $ space) w_parameter parameters
    $ close_box

  let w_crlf k e = string "\r\n" k e

  let w_field = function
    | `ContentType t ->
      string "Content-Type: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_content t $ close_box) (unlift k))))
      $ w_crlf
end

module Decoder =
struct
  let p_content = Rfc2045.content
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

  aux 0 @@ Parser.run i Parser.(Rfc2045.content <* Rfc822.crlf)

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

  aux off @@ Parser.run i Rfc2045.content
