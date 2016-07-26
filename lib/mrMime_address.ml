type word           = Rfc822.word
type local          = Rfc822.local
type raw            = Rfc2047.raw = QuotedPrintable of string | Base64 of MrMime_base64.result
type phrase         = Rfc5322.phrase
type domain         = Rfc5322.domain
type literal_domain = Rfc5321.literal_domain = ..
type Rfc5321.literal_domain += IPv4 = Rfc5321.IPv4
type Rfc5321.literal_domain += IPv6 = Rfc5321.IPv6

type mailbox = Rfc5322.mailbox =
  { name    : phrase option
  ; local   : Rfc822.local
  ; domain  : domain * domain list }

type group = Rfc5322.group =
  { name    : phrase
  ; mailbox : mailbox list }

type address = [ `Group of group | `Mailbox of mailbox ]

(* convenience alias *)
module QuotedPrintable = MrMime_quotedPrintable
module Base64          = MrMime_base64
module Input           = MrMime_input

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_word fmt = function
  | `Atom x -> pp fmt "`Atom \"%s\"" x
  | `String s -> pp fmt "`String %S" s

let pp_domain fmt (x : domain) = match x with
  | `Domain lst ->
    pp fmt "[@[<hov>%a@]]"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@ .@ ") (fun fmt x -> pp fmt "\"%s\"" x)) lst
  | `Literal (Rfc5321.IPv4 ipv4) ->
    pp fmt "[@[<hov>%s@]]" (Ipaddr.V4.to_string ipv4)
  | `Literal (Rfc5321.IPv6 ipv6) ->
    pp fmt "[@[<hov>%s@]]" (Ipaddr.V6.to_string ipv6)

let pp_raw fmt = function
  | Rfc2047.QuotedPrintable raw -> pp fmt "quoted-printable:%s" raw
  | Rfc2047.Base64 (`Clean raw) -> pp fmt "base64:%s" raw
  | Rfc2047.Base64 (`Dirty raw) -> pp fmt "base64:%S" raw
  | Rfc2047.Base64 `Wrong_padding -> pp fmt "base64:wrong-padding"

let pp_phrase fmt =
  let rec aux fmt = function
    | `Dot    -> pp fmt "."
    | `Word x -> pp fmt "`Word (%a)" pp_word x
    | `Encoded (charset, raw) ->
        pp fmt "{@[<hov>charset = %s;@ raw = %a@]}" charset pp_raw raw
  in
  pp fmt "[@[<hov>%a@]]" (pp_lst ~sep:(fun fmt () -> pp fmt "@ ") aux)

let pp_local fmt local =
  let pp_word fmt x = pp fmt "(%a)" pp_word x in
  pp fmt "[@[<hov>%a@]]"
    (pp_lst ~sep:(fun fmt () -> pp fmt "@ .@ ") pp_word) local

let pp_mailbox' fmt (local, (first, rest)) =
  match rest with
  | [] ->
    pp fmt "{@[<hov>local = %a;@ domain = %a@]}"
      pp_local local
      pp_domain first
  | lst ->
    pp fmt "{@[<hov>local = %a;@ domain = [@[<hov>%a;@ %a@]]@]}"
      pp_local local
      pp_domain first
      (pp_lst ~sep:(fun fmt () -> pp fmt ";@ ") pp_domain) lst

let pp_mailbox fmt = function
  | { Rfc5322.name = Some name; local; domain; } ->
    pp fmt "{@[<hov>%a = %a@]}"
      pp_phrase name
      pp_mailbox' (local, domain)
  | { Rfc5322.name = None; local; domain;} ->
    pp fmt "@[<hov>%a@]"
      pp_mailbox' (local, domain)

let pp_group fmt { Rfc5322.name; mailbox; } =
  pp fmt "{@[<hov>%a = %a;@]}"
    pp_phrase name
    (pp_lst ~sep:(fun fmt () -> pp fmt ";@ ") pp_mailbox) mailbox

let pp fmt = function
  | `Group g -> pp fmt "`Group %a" pp_group g
  | `Mailbox m -> pp fmt "`Mailbox %a" pp_mailbox m

module Encoder =
struct
  include Encoder
  open Wrap

  let w_atom = function
    | `Atom s -> hovbox 0 $ string s $ close_box

  let w_domain = function
    | `Domain lst ->
      let rec aux = function
        | [] -> noop
        | [ x ] -> string x
        | x :: r -> string x $ hovbox 0 $ string "." $ aux r $ close_box
      in hovbox 0 $ aux lst $ close_box
    | `Literal (Rfc5321.IPv4 ipv4) ->
      hovbox 0
      $ string "["
      $ string (Ipaddr.V4.to_string ipv4)
      $ string "]"
      $ close_box
    | `Literal (Rfc5321.IPv6 ipv6) ->
      hovbox 0
      $ string "[IPv6:"
      $ string (Ipaddr.V6.to_string ipv6)
      $ string "]"
      $ close_box

  let explode str =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (str.[i] :: l) in
    exp (String.length str - 1) []

  let w_safe_string str =
    let is_vchar = function
      | '\x21' .. '\x7e' -> true
      | _ -> false
    in
    List.fold_right
      (function
       | '\x00' -> string "\\\000"
       | '\x07' -> string "\\a"
       | '\x08' -> string "\\b"
       | '\x09' -> string "\\t"
       | '\x0A' -> string "\\n"
       | '\x0B' -> string "\\v"
       | '\x0C' -> string "\\f"
       | '\x0D' -> string "\\r"
       | '\\'   -> string "\\\\"
       | '"'    -> string "\\\""
       | ' '    -> string " "
       | '\x00' .. '\x7F' as chr ->
         if is_vchar chr
         then char chr
         else string (sp "\\%c" chr)
       | chr -> char chr)
      (explode str)

  let w_word = function
    | `Atom str -> w_atom (`Atom str)
    | `String str ->
      hovbox 1
      $ char '"'
      $ w_safe_string str
      $ char '"'
      $ close_box

  let wrap a =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> string (Buffer.contents buf)
    in

    loop @@ (a (fun _ -> `Ok) (Encoder.make ()))

  let w_raw = function
    | Rfc2047.QuotedPrintable s ->
      string "Q?" $ (wrap (QuotedPrintable.w_inline_encode s $ Encoder.flush))
    | Rfc2047.Base64 (`Clean raw)
    | Rfc2047.Base64 (`Dirty raw) -> string "B?" $ (wrap (Base64.w_inline_encode raw $ Encoder.flush))
    | Rfc2047.Base64 `Wrong_padding -> string "B?"

  let rec w_lst w_sep w_data l =
    let open Wrap in
      let rec aux = function
      | [] -> noop
      | [ x ] -> w_data x
      | x :: r -> w_data x $ w_sep $ aux r
    in aux l

  let w_phrase phrase =
    let w_element = function
       | `Dot -> char '.'
       | `Word x -> w_word x
       | `Encoded (charset, raw) ->
          hovbox 0
          $ string "=?" $ string charset $ string "?"
          $ w_raw raw $ string "?="
          $ close_box
    in
    w_lst space w_element phrase

  let rec w_local = function
    | [] -> noop
    | [ x ] -> w_word x
    | x :: r -> w_word x $ string "." $ w_local r

  let w_mailbox' (local, (domain, rest)) =
    match rest with
    | [] ->
      hovbox 0
      $ hovbox 0
      $ w_local local
      $ close_box
      $ string "@"
      $ hovbox 0
      $ w_domain domain
      $ close_box
      $ close_box
    | domains ->
      let rec aux = function
        | [] -> noop
        | [ x ] -> w_domain x
        | x :: r -> w_domain x $ string "," $ aux r
      in
      hovbox 0
      $ string "@"
      $ hovbox 0
      $ aux domains
      $ close_box
      $ string ":"
      $ hovbox 0
      $ w_local local
      $ close_box
      $ string "@"
      $ hovbox 0
      $ w_domain domain
      $ close_box
      $ close_box

  let w_mailbox { name; local; domain; } =
    match name with
    | Some name ->
      hovbox 0
      $ hovbox 0
      $ w_phrase name
      $ close_box
      $ space
      $ hovbox 0
      $ string "<"
      $ w_mailbox' (local, domain)
      $ string ">"
      $ close_box
      $ close_box
    | None ->
      hovbox 0
      $ w_mailbox' (local, domain)
      $ close_box

  let w_group { name; mailbox; } =
    let rec aux = function
      | [] -> noop
      | [ x ] -> w_mailbox x
      | x :: r -> hovbox 0 $ w_mailbox x $ close_box $ string "," $ space $ aux r
    in
    hovbox 0
    $ hovbox 0
    $ w_phrase name
    $ close_box
    $ string ":"
    $ space
    $ hovbox 0
    $ aux mailbox
    $ close_box
    $ string ";"
    $ close_box

  let w_address = function
    | `Group g -> w_group g
    | `Mailbox m -> w_mailbox m

  let w_addresses lst =
    let rec aux = function
      | [] -> noop
      | [ x ] -> w_address x
      | x :: r -> w_address x $ string "," $ hovbox 0 $ space $ aux r $ close_box
    in hovbox 0 $ aux lst $ close_box
end

module Decoder =
struct
  let p_address   = Rfc5322.address
  let p_addresses = Rfc5322.address_list
  let p_local     = Rfc822.local_part
  let p_domain    = Rfc5322.domain
end

let to_string t =
  let buf   = Buffer.create 16 in
  let state = Encoder.make () in

  let rec loop = function
    | `Partial (s, i, l, k) ->
      Buffer.add_subbytes buf s i l;
      loop @@ (k l)
    | `Ok -> Buffer.contents buf
  in

  loop @@ (Wrap.lift Wrap.(Encoder.w_address t (unlift (Encoder.flush (fun _ -> `Ok))))) state

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

  aux 0 @@ Parser.run i Parser.(Rfc5322.address <* Rfc822.crlf)

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

  aux off @@ Parser.run i Rfc5322.address

let equal = (=)

module List =
struct
  let pp fmt =
    pp_lst ~sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp fmt

  let to_string t =
    let buf   = Buffer.create 16 in
    let state = Encoder.make () in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> Buffer.contents buf
    in

    loop @@ (Wrap.lift Wrap.(Encoder.w_addresses t (unlift (Encoder.flush (fun v -> `Ok))))) state

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

    aux 0 @@ Parser.run i Parser.(Rfc5322.address_list <* Rfc822.crlf)

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

    aux off @@ Parser.run i Rfc5322.address_list

  let equal = (=)
end

module Extension =
struct
  let ldh_str s =
    let l = String.length s in
    let i = ref 0 in

    let ldh_chr = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false in
    let let_dig = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false in

    while !i < l && ldh_chr (String.get s !i) do incr i done;

    !i = l || (!i + 1 = l && let_dig (String.get s (!i + 1)))

  let add_literal_domain tag literal_domain =
    if String.length tag >= 1
       && ldh_str tag
    then Hashtbl.add Rfc5321.iana_hashtbl tag literal_domain
    else raise (Invalid_argument "Address.Extension.add_literal_domain: bad tag")
end
