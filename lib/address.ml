open Base

type domain   = [ Rfc5322.domain | LiteralDomain.t ]
type local    = Rfc5322.local
type encoding = Rfc2047.encoding = QuotedPrintable | Base64
type phrase   = Rfc5322.phrase

let size_of_local =
  List.fold_left (fun acc -> function
    | `Atom s -> String.length s + acc
    | `String s -> String.length s + acc) 0

let size_of_domain domain =
  let aux = List.fold_left (fun acc -> function `Atom s -> String.length s + acc) 0 in
  match domain with
  | `Domain l -> aux l
  | `Literal s -> String.length s
  | #LiteralDomain.t as l -> LiteralDomain.size l

type mailbox =
  { local   : local
  ; domain  : domain * domain list }

type person =
  { name    : Rfc5322.phrase option
  ; mailbox : mailbox }

type group =
  { name    : Rfc5322.phrase
  ; persons : person list }

type t = [ `Group of group | `Person of person ]

let pp_domain fmt = function
  | `Domain l -> p fmt "%a" (pp_list ~sep:"." pp_atom) l
  | `Literal s -> p fmt "[%a]" (pp_string ~in_qs:false ~in_dm:true) s
  | #LiteralDomain.t as l -> p fmt "[%a]" LiteralDomain.pp l

let pp_local fmt =
  p fmt "%a" (pp_list ~sep:"." pp_word)

let pp_mailbox fmt { local; domain = (one, rest); } =
  let pp_ext fmt = function
    | [] -> ()
    | l  -> p fmt "@%a:" (pp_list ~sep:"," pp_domain) l
  in
  p fmt "%a%a@%a" pp_ext rest pp_local local pp_domain one

let pp_person fmt { name; mailbox; } =
  match name with
  | Some name -> p fmt "%a <%a>" pp_phrase name pp_mailbox mailbox
  | None -> p fmt "%a" pp_mailbox mailbox

let pp_group fmt { name; persons; } =
  p fmt "%a: %a;" pp_phrase name (pp_list ~sep:"," pp_person) persons

let pp fmt = function
  | `Group group -> pp_group fmt group
  | `Person person -> pp_person fmt person

let domain_of_lexer ?(relax = true) = function
  | `Domain a -> `Domain a
  | `Literal s ->
    try (LiteralDomain.of_string ~relax s :> domain)
    with exn -> if relax then `Literal s else raise exn

let mailbox_of_lexer ?(relax = true) (local, domains) =
  let domains = List.map (domain_of_lexer ~relax) domains in

  let first, rest = match domains with
    | first :: rest -> first, rest
    | _ -> raise (Invalid_argument "Address.mailbox_of_lexer")
  in

  let size_of_rest =
    List.length rest = 0
    || List.for_all (fun x -> x > 0) (List.map size_of_domain rest)
  in

  if size_of_local local > 0
  && size_of_domain first > 0
  && size_of_rest
  then { local; domain = (first, rest); }
  else raise (Invalid_argument "Address.mailbox_of_lexer")

let person_of_lexer ?(relax = true) (name, mailbox) =
  { name; mailbox = mailbox_of_lexer ~relax mailbox; }

let group_of_lexer ?(relax = true) (name, persons) =
  { name; persons = List.map (person_of_lexer ~relax) persons; }

let of_lexer ?(relax = true) = function
  | `Group group -> `Group (group_of_lexer ~relax group)
  | `Person person -> `Person (person_of_lexer ~relax person)

let of_string ?(relax = true) s =
  let rec loop = function
    | `Error (exn, buf, off, len) ->
      let tmp = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer tmp in

      Format.fprintf fmt "%a (buf: %S)%!"
        Lexer.pp_error exn (Bytes.sub buf off (len - off));

      raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
    | `Read (buf, off, len, k) ->
      raise (Invalid_argument "Address.of_string: unterminated string")
    | `Ok data -> of_lexer ~relax data
  in

  let rule = Rfc5322.p_address
    (fun data -> Rfc5322.p_crlf (fun _ -> `Ok data)) in
  loop @@ Lexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n"))

let to_string t =
  let tmp = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer tmp in

  Format.fprintf fmt "%a%!" pp t;
  Buffer.contents tmp

let equal = (=)

module List =
struct
  type nonrec t = t list

  let of_lexer ?(relax = true) = List.map (of_lexer ~relax)

  let of_string ?(relax = true) s =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Lexer.pp_error exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("Address.List.of_string: "
                                 ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Address.List.of_string: unterminated string")
      | `Ok data -> of_lexer ~relax data
    in

    let rule = Rfc5322.p_address_list
      (fun data -> Rfc5322.p_crlf (fun _ -> `Ok data)) in
    loop @@ Lexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n"))

  let pp fmt =
    p fmt "%a" (pp_list ~sep:", " pp)

  let to_string t =
    let tmp = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer tmp in

    Format.fprintf fmt "%a%!" pp t;
    Buffer.contents tmp

  let equal = (=)
end
