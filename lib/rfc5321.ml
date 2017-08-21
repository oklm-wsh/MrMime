type literal_domain = ..
type literal_domain += IPv4 of Ipaddr.V4.t
type literal_domain += IPv6 of Ipaddr.V6.t

open Parser
open Parser.Convenience

type err += Invalid_ipv4

let ipv4_address_literal =
  { f = fun i s fail succ ->
    let r = ref None in
    let _ = Input.transmit i
      (fun buff off _len ->
       let i = ref off in
       try r := Some (Ipaddr.V4.of_string_raw (Internal_buffer.to_string buff) i);
           (!i - off)
       with Ipaddr.Parse_error _ -> 0) in

    match !r, Input.ravailable i with
    | Some ipv4, 0 -> succ i s ipv4
    | _ -> fail i s [] Invalid_ipv4 }

type err += Invalid_ipv6

let ipv6_addr =
  { f = fun i s fail succ ->
    let r = ref None in
    let _ = Input.transmit i
      (fun buff off _len ->
       let i = ref off in
       try r := Some (Ipaddr.V6.of_string_raw (Internal_buffer.to_string buff) i);
           (!i - off)
       with Ipaddr.Parse_error _ -> 0) in

    match !r, Input.ravailable i with
    | Some ipv6, 0 -> succ i s ipv6
    | _ -> fail i s [] Invalid_ipv6 }

let ipv6_address_literal =
  let string s = string (fun x -> x) s in
  string "IPv6:" *> ipv6_addr

let implode l =
  let s = Bytes.create (List.length l) in
  let rec aux i = function
    | [] -> s
    | x :: r -> Bytes.set s i x; aux (i + 1) r
  in
  aux 0 l

let let_dig = satisfy (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

let ldh_str =
  many (satisfy (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false))
  >>= fun lst -> let_dig >>| fun rst -> implode (lst @ [ rst ])

let is_dcontent = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | _ -> false

type err += Invalid_address_literal of string | Unknown_tag of string

let iana_hashtbl : (bytes, literal_domain t) Hashtbl.t = Hashtbl.create 16
let () = Hashtbl.add iana_hashtbl (Bytes.of_string "IPv6") (ipv6_addr >>| fun v -> IPv6 v)

let general_address_literal =
  ldh_str
  >>= fun tag -> char ':'
  *> one (satisfy is_dcontent) >>| implode
  >>= fun content ->
    { f = fun i s fail succ ->
      try let p = Hashtbl.find iana_hashtbl tag in
          let b = Input.create_by ~proof:(Input.proof i) (Bytes.length content) in

          Input.write b (Internal_buffer.from_string ~proof:(Input.proof i) (Bytes.to_string content)) 0 (Bytes.length content);
          match only b p with
          | Read _ | Fail _ -> fail i s [] (Invalid_address_literal (Bytes.to_string tag))
          | Done v -> succ i s v
      with Not_found -> fail i s [] (Unknown_tag (Bytes.to_string tag)) }

let address_literal =
  (ipv4_address_literal >>| fun v -> IPv4 v)
  <|> (ipv6_address_literal >>| fun v -> IPv6 v)
  <|> general_address_literal
