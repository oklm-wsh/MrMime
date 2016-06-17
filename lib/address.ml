type atom     = Rfc5322.atom
type word     = Rfc5322.word
type domain   = [ `Domain of atom list | `Literal of string list | LiteralDomain.t ]
type local    = Rfc5322.local
type encoding = Rfc2047.encoding = QuotedPrintable | Base64
type phrase   = Rfc5322.phrase

let pp = Format.fprintf

let pp_atom fmt = function
  | `Atom x -> pp fmt "%s" x

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_word fmt = function
  | `Atom x -> pp fmt "%s" x
  | `String s -> pp fmt "%S" s

let pp_domain fmt = function
  | `Domain lst -> pp fmt "[@[<hov>%a@]]" (pp_lst ~sep:(fun fmt () -> pp fmt ".") pp_atom) lst
  | `Literal lst -> pp fmt "[@[<hov>%a@]]" (pp_lst ~sep:(fun fmt () -> pp fmt "@ ") (fun fmt s -> pp fmt "%s" s)) lst
  | #LiteralDomain.t as x -> LiteralDomain.pp fmt x

let pp_local =
  pp_lst ~sep:(fun fmt () -> pp fmt ".") pp_word

let pp_encoding fmt = function
  | QuotedPrintable -> pp fmt "Q"
  | Base64 -> pp fmt "B"

let pp_phrase fmt =
  let make f n =
    let rec aux acc = function
      | 0 -> List.rev acc
      | n -> aux (f n :: acc) (n - 1)
    in aux [] n
  in

  let rec aux fmt = function
    | `Atom x -> pp fmt "%s" x
    | `CR n -> pp fmt "%s" (String.concat "" (make (fun _ -> "\r") n))
    | `Dot -> pp fmt "."
    | `Encoded (charset, encoding, data) ->
      pp fmt "{ @[<hov> charset = %s;@ encoding = %a;@ data = %S@] }"
        charset pp_encoding encoding data
    | `FWS -> pp fmt "<fws>"
    | `LF n -> pp fmt "%s" (String.concat "" (make (fun _ -> "\n") n))
    | `String s -> pp fmt "%S" s
    | `WSP -> pp fmt "@ "
  in

  pp fmt "@[<hov>%a@]" (pp_lst ~sep:(fun fmt () -> pp fmt "@,") aux)

let size_of_local =
  List.fold_left (fun acc -> function
    | `Atom s -> String.length s + acc
    | `String s -> String.length s + acc) 0

let size_of_domain domain =
  let aux = List.fold_left (fun acc -> function `Atom s -> String.length s + acc) 0 in
  match domain with
  | `Domain l -> aux l
  | `Literal l -> List.fold_left (fun acc x -> acc + (String.length x)) 0 l
  | #LiteralDomain.t as l -> LiteralDomain.size l

type mailbox =
  { local   : local
  ; domain  : domain * domain list }

type person =
  { name    : phrase option
  ; mailbox : mailbox }

type group =
  { name    : phrase
  ; persons : person list }

type t = [ `Group of group | `Person of person ]

let pp_mailbox fmt { local; domain = (first, rest) } =
  match rest with
  | [] -> pp fmt "{ @[<hov>%a;@ %a@] }" pp_local local pp_domain first
  | lst -> pp fmt "{ @[<hov>%a;@ @[<hov>%a@ |@ %a@]@] }"
    pp_local local pp_domain first (pp_lst ~sep:(fun fmt () -> pp fmt "@ |@ ") pp_domain) lst

let pp_person fmt = function
  | { name = Some name; mailbox; } ->
      pp fmt "@[<hov>%a:@ %a@]" pp_phrase name pp_mailbox mailbox
  | { name = None; mailbox; } ->
      pp fmt "@[<hov>%a@]" pp_mailbox mailbox

let pp_group fmt { name; persons; } =
  pp fmt "@[<hov>%a:@ %a;@]"
    pp_phrase name (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") pp_person) persons

let pp fmt = function
  | `Group grp -> pp_group fmt grp
  | `Person prs -> pp_person fmt prs

module D =
struct
  let domain_of_lexer x = (x :> domain)

  let mailbox_of_lexer (local, domains) =
    let domains = List.map domain_of_lexer domains in

    let first, rest = match domains with
      | first :: rest -> first, rest
      | _ -> raise (Invalid_argument "Address.mailbox_of_lexer")
    in

    if size_of_local local > 0
    && size_of_domain first > 0
    && List.for_all (fun x -> x > 0) (List.map size_of_domain rest)
    then { local; domain = (first, rest); }
    else raise (Invalid_argument "Address.mailbox_of_lexer")

  let person_of_lexer (name, mailbox) =
    { name; mailbox = mailbox_of_lexer mailbox; }

  let group_of_lexer (name, persons) =
    { name; persons = List.map person_of_lexer persons; }

  let of_lexer = function
    | `Group group -> `Group (group_of_lexer group)
    | `Person person -> `Person (person_of_lexer person)

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

    let rule =
      Rfc5322.p_address
      @ fun data -> Rfc822.p_crlf
      @ Rfc822.p_crlf
      @ fun _ -> `Ok data
    in

    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_atom = function
      | `Atom s -> w_hovbox 1 $ w_string s $ w_close_box

    let w_domain = function
      | `Domain lst ->
        let rec aux = function
          | [] -> noop
          | [ x ] -> w_atom x
          | x :: r -> w_atom x $ w_hovbox 1 $ w_string "." $ aux r $ w_close_box
        in w_hovbox 1 $ aux lst $ w_close_box
      | `Literal l ->
        w_hovbox 1
        $ w_string "["
          (* TODO: may be we must use LiteralDomain.E.w_safe_string to escape ]
                   character *)
        $ List.fold_right (fun x k -> w_hovbox 1 $ w_string x $ w_close_box $ k) l noop
        $ w_string "]"
        $ w_close_box
      | #Rfc5321.literal_domain as x -> LiteralDomain.E.w x

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
         | '\x00' -> w_string "\\\000"
         | '\x07' -> w_string "\\a"
         | '\x08' -> w_string "\\b"
         | '\x09' -> w_string "\\t"
         | '\x0A' -> w_string "\\n"
         | '\x0B' -> w_string "\\v"
         | '\x0C' -> w_string "\\f"
         | '\x0D' -> w_string "\\r"
         | '\\'   -> w_string "\\\\"
         | '"'    -> w_string "\\\""
         | ' '    -> w_string " "
         | '\x00' .. '\x7F' as chr ->
           if is_vchar chr
           then w_char chr
           else w_string (sp "\\%c" chr)
         | chr -> w_char chr)
        (explode str)

    let w_word = function
      | #Rfc5322.atom as atom -> w_atom atom
      | `String str ->
        w_hovbox 1
        $ w_char '"'
        $ w_safe_string str
        $ w_char '"'
        $ w_close_box

    let w_encoding = function
      | Rfc2047.QuotedPrintable -> w_char 'Q'
      | Rfc2047.Base64 -> w_char 'B'

    let wrap a =
      let buf = Buffer.create 16 in

      let rec loop = function
        | `Partial (s, i, l, k) ->
          Buffer.add_subbytes buf s i l;
          loop @@ (k l)
        | `Ok -> w_string (Buffer.contents buf)
      in

      loop @@ (a (fun _ -> `Ok) (Encoder.make ()))

    let w_phrase phrase =
      List.fold_right
          (function
           | #Rfc5322.atom as atom -> w_atom atom
           | `String str -> w_hovbox 1 $ w_char '"' $ w_safe_string str $ w_char '"' $ w_close_box
           | `Dot -> w_char '.'
           | `WSP -> w_space
           | `FWS -> w_force_newline $ w_space
           | `CR n -> w_string (String.make n '\r')
           | `LF n -> w_string (String.make n '\n')
           | `Encoded (charset, encoding, data) ->
             w_hovbox 1
             $ wrap (Rfc2047.w_decoded_word charset encoding data $ BaseEncoder.flush)
             $ w_close_box)
          phrase

    let rec w_local = function
      | [] -> noop
      | [ x ] -> w_word x
      | x :: r -> w_word x $ w_string "." $ w_local r

    let w_mailbox { local; domain = (domain, rest); } =
      match rest with
      | [] ->
        w_hovbox 1
        $ w_hovbox 1
        $ w_local local
        $ w_close_box
        $ w_string "@"
        $ w_hovbox 1
        $ w_domain domain
        $ w_close_box
        $ w_close_box
      | domains ->
        let rec aux = function
          | [] -> noop
          | [ x ] -> w_domain x
          | x :: r -> w_domain x $ w_string "," $ aux r
        in
        w_hovbox 1
        $ w_string "@"
        $ w_hovbox 1
        $ aux domains
        $ w_close_box
        $ w_string ":"
        $ w_hovbox 1
        $ w_local local
        $ w_close_box
        $ w_string "@"
        $ w_hovbox 1
        $ w_domain domain
        $ w_close_box
        $ w_close_box

    let w_person { name; mailbox; } =
      match name with
      | Some name ->
        w_hovbox 1
        $ w_hovbox 1
        $ w_phrase name
        $ w_close_box
        $ w_space
        $ w_hovbox 1
        $ w_string "<"
        $ w_mailbox mailbox
        $ w_string ">"
        $ w_close_box
        $ w_close_box
      | None ->
        w_hovbox 1
        $ w_mailbox mailbox
        $ w_close_box

    let w_group { name; persons; } =
      let rec aux = function
        | [] -> noop
        | [ x ] -> w_person x
        | x :: r -> w_hovbox 1 $ w_person x $ w_close_box $ w_string "," $ w_space $ aux r
      in
      w_hovbox 1
      $ w_hovbox 1
      $ w_phrase name
      $ w_close_box
      $ w_string ":"
      $ w_space
      $ w_hovbox 1
      $ aux persons
      $ w_close_box
      $ w_string ";"
      $ w_close_box

    let w_address = function
      | `Group g -> w_group g
      | `Person p -> w_person p
  end

  let w         = Internal.w_address
  let w_word    = Internal.w_word
  let w_local   = Internal.w_local
  let w_domain  = Internal.w_domain
  let w_phrase  = Internal.w_phrase
  let w_person  = Internal.w_person
  let w_mailbox = Internal.w_mailbox
  let w_safe_string = Internal.w_safe_string

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
      Wrap.lift Wrap.(Internal.w_address t (unlift ok))
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string (s ^ "\r\n\r\n"))
let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

let equal = (=)

module List =
struct
  module D =
  struct
    let of_lexer = List.map D.of_lexer

    open BaseDecoder

    let of_decoder state =
      let rec loop = function
        | `Error (exn, buf, off, len) ->
          let tmp = Buffer.create 16 in
          let fmt = Format.formatter_of_buffer tmp in

          Format.fprintf fmt "%a (buf: %S)%!"
            Error.pp exn (Bytes.sub buf off (len - off));

          raise (Invalid_argument ("Address.List.of_string: "
                                   ^ (Buffer.contents tmp)))
        | `Read (buf, off, len, k) ->
          raise (Invalid_argument "Address.List.of_string: unterminated string")
        | `Ok data -> of_lexer data
      in

      let rule =
        Rfc5322.p_address_list
        @ fun data -> Rfc822.p_crlf
        @ Rfc822.p_crlf
        @ fun _ -> `Ok data
      in

      loop @@ safe rule state
  end

  module E =
  struct
    module Internal =
    struct
      open BaseEncoder
      open Wrap

      let w_lst w_sep w_data lst =
        let rec aux = function
          | [] -> noop
          | [ x ] -> w_data x
          | x :: r -> w_data x $ w_sep $ aux r
        in aux lst

      let w_addresses lst =
        w_hovbox 1
        $ w_lst (w_string "," $ w_space) E.w lst
        $ w_close_box
    end

    let w = Internal.w_addresses

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
        Wrap.lift Wrap.(Internal.w_addresses t (unlift ok))
      in

      loop @@ rule state
  end

  let of_string s = D.of_decoder (Decoder.of_string (s ^ "\r\n\r\n"))
  let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

  let equal = (=)

  let pp fmt lst =
    pp_lst ~sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp fmt lst
end
