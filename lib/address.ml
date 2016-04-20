open Base

type domain  = Rfc5322.domain
type local   = Rfc5322.local

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

external domain_of_lexer : Rfc5322.domain -> domain = "%identity"

let mailbox_of_lexer (local, domains) =
  let first, rest = match domains with
    | first :: rest -> first, rest
    | _ -> raise (Invalid_argument "Address.mailbox_of_lexer")
  in
  { local; domain = (first, rest); }

let person_of_lexer (name, mailbox) =
  { name; mailbox = mailbox_of_lexer mailbox; }

let group_of_lexer (name, persons) =
  { name; persons = List.map person_of_lexer persons; }

let of_lexer = function
  | `Group group -> `Group (group_of_lexer group)
  | `Person person -> `Person (person_of_lexer person)

let of_string s =
  let rec loop = function
    | `Error (exn, buf, off, len) ->
      let tmp = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer tmp in

      Format.fprintf fmt "%a (buf: %S)%!"
        Lexer.pp_error exn (Bytes.sub buf off (len - off));

      raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
    | `Read (buf, off, len, k) ->
      raise (Invalid_argument "Address.of_string: unterminated string")
    | `Ok data -> of_lexer data
  in

  let rule = Rfc5322.p_address (fun data -> Rfc5322.p_crlf (fun _ -> `Ok data)) in
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

  let of_lexer = List.map of_lexer

  let of_string s =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Lexer.pp_error exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("Address.List.of_string: " ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Address.List.of_string: unterminated string")
      | `Ok data -> of_lexer data
    in

    let rule = Rfc5322.p_address_list (fun data -> Rfc5322.p_crlf (fun _ -> `Ok data)) in
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
