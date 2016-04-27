module M = Tyxml_js.Html5
module R = Tyxml_js.R

let hljs = Js.Unsafe.variable "hljs"

let e_ipv4 =
  let open Json_encoding in
  conv
    Ipaddr.V4.to_string
    Ipaddr.V4.of_string_exn
    string

let e_ipv6 =
  let open Json_encoding in
  conv
    Ipaddr.V6.to_string
    Ipaddr.V6.of_string_exn
    string

let e_atom =
  let open Json_encoding in
  let atom = case string (function `Atom s -> Some s) (fun s -> `Atom s) in
  union [ atom ]

let e_domain : Address.domain Json_encoding.encoding =
  let open Json_encoding in
  let domain  = case (list e_atom) (function `Domain l -> Some l | _ -> None) (fun l -> `Domain l) in
  let general = case (tup2 string string) (function `General g -> Some g | _ -> None) (fun g -> `General g) in
  let ipv4    = case e_ipv4 (function `IPv4 ipv4 -> Some ipv4 | _ -> None) (fun i -> `IPv4 i) in
  let ipv6    = case e_ipv6 (function `IPv6 ipv6 -> Some ipv6 | _ -> None) (fun i -> `IPv6 i) in
  let literal = case string (function `Literal s -> Some s | _ -> None) (fun s -> `Literal s) in
  union [domain; general; ipv4; ipv6; literal]

let e_local : Address.local Json_encoding.encoding =
  let open Json_encoding in
  let atom   = case string (function `Atom s -> Some s | _ -> None) (fun s -> `Atom s) in
  let str    = case string (function `String s -> Some s | _ -> None) (fun s -> `String s) in
  list (union [atom; str])

let e_mailbox : Address.mailbox Json_encoding.encoding =
  let open Json_encoding in
  conv
    (fun { Address.local; domain; } -> (local, domain))
    (fun (local, domain) -> { Address.local; domain; })
    (obj2
      (req "local" e_local)
      (req "domain" (tup2 e_domain (list e_domain))))

let e_phrase : Address.phrase Json_encoding.encoding =
  let open Json_encoding in
  let e_encoding : Address.encoding Json_encoding.encoding =
    string_enum
      [ "Q", Address.QuotedPrintable
      ; "B", Address.Base64 ]
  in
  let e_encoded : (string * Address.encoding * string) Json_encoding.encoding =
    obj3
      (req "charset" string)
      (req "encoding" e_encoding)
      (req "encoded" string)
  in
  let atom    = case string (function `Atom s -> Some s | _ -> None) (fun s -> `Atom s) in
  let dot     = case string (function `Dot -> Some "." | _ -> None) (fun _ -> `Dot) in
  let encoded = case e_encoded (function `Encoded e -> Some e | _ -> None) (fun e -> `Encoded e) in
  let str     = case string (function `String s -> Some s | _ -> None) (fun s -> `String s) in
  let wsp     = case string (function `WSP -> Some " " | _ -> None) (fun _ -> `WSP) in
  list (union [atom; dot; encoded; str; wsp])

let e_person : Address.person Json_encoding.encoding =
  let open Json_encoding in
  conv
    (fun { Address.name; mailbox; } -> (name, mailbox))
    (fun (name, mailbox) -> { Address.name; mailbox; })
    (obj2
      (opt "name" e_phrase)
      (req "mailbox" e_mailbox))

let e_group : Address.group Json_encoding.encoding =
  let open Json_encoding in
  conv
    (fun { Address.name; persons; } -> (name, persons))
    (fun (name, persons) -> { Address.name; persons; })
    (obj2
      (req "name" e_phrase)
      (dft "persons" (list e_person) []))

let e : Address.t Json_encoding.encoding =
  let open Json_encoding in
  let group = case e_group (function `Group g -> Some g | _ -> None) (fun g -> `Group g) in
  let person = case e_person (function `Person p -> Some p | _ -> None) (fun p -> `Person p) in
  union [group; person]

let get coerce id =
  Js.Opt.get
    (Dom_html.getElementById id |> coerce)
    (fun () -> assert false)

type mode = [ `None | `Error of exn | `Valid of Address.t ]

let current, set = React.S.create (`None : [> mode])
let (>|~=) x f = React.S.map f x

let ok =
  let gok, sok = ReactiveData.RList.create [] in
  let classes = current >|~= function
     | `Valid _ -> ["ok"]
     | `Error _ | `None -> ["ok"; "hidden"]
  in
  let _ = current >|~= function
    | `Valid m ->
      let json = Ezjsonm.to_string @@ Ezjsonm.wrap @@ Json_encoding.construct e m in
      ReactiveData.RList.set sok
        [ M.pre [ M.code ~a:[M.a_class ["json"]] [ M.pcdata (Yojson.Safe.prettify json) ] ] ]
    | `Error _ | `None -> ReactiveData.RList.set sok []
  in
  let _ =
    ReactiveData.RList.map
      (fun pre -> hljs##highlightBlock (Tyxml_js.To_dom.of_pre pre))
      gok
  in
  R.Html5.div ~a:[R.Html5.a_class classes] gok

let error =
  let gerr, serr = ReactiveData.RList.create [] in
  let classes = current >|~= function
     | `Error _ -> ["error"]
     | `Valid _ | `None -> ["error"; "hidden"]
  in
  let _ = current >|~= function
    | `Error err ->
      ReactiveData.RList.set serr [ M.b [ M.pcdata "Invalid Email" ] ]
    | `Valid _ | `None -> ReactiveData.RList.set serr []
  in
  R.Html5.div ~a:[R.Html5.a_class classes] gerr

let button = get Dom_html.CoerceTo.button "validate-button"
let term   = get Dom_html.CoerceTo.input "validate-term"

let main () =
  let data = term##.value |> Js.to_string in
  try let m = Address.of_string ~relax:false data in
      set (`Valid m)
  with exn -> set (`Error exn)

let () =
  let body = get Dom_html.CoerceTo.body "main" in

  Dom.appendChild body (Tyxml_js.To_dom.of_div ok);
  Dom.appendChild body (Tyxml_js.To_dom.of_div error);

  let _ = Dom_html.addEventListener
    button Dom_html.Event.click
    (Dom_html.handler (fun ev -> main (); Js.bool true))
    (Js.bool true)
  in ()
