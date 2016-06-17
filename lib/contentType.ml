type ty    = Rfc2045.ty
type subty = Rfc2045.subty
type value = Rfc2045.value

let pp = Format.fprintf

let pp_ty fmt = function
  | `Audio        -> pp fmt "audio"
  | `Ietf_token s -> pp fmt "ietf-token:%s" s
  | `Application  -> pp fmt "application"
  | `X_token s    -> pp fmt "x-token:%s" s
  | `Message      -> pp fmt "message"
  | `Image        -> pp fmt "image"
  | `Video        -> pp fmt "video"
  | `Multipart    -> pp fmt "multipart"
  | `Text         -> pp fmt "text"

let pp_subty fmt = function
  | `Ietf_token s -> pp fmt "ietf-token:%s" s
  | `X_token s    -> pp fmt "x-token:%s" s
  | `Iana_token s -> pp fmt "iana-token:%s" s

let pp_value fmt = function
  | `String s -> pp fmt "%S" s
  | `Token s -> pp fmt "%s" s

type t =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

let pp_lst pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a;@ " pp_data x; aux r
  in

  pp fmt "[@[<hov>"; aux lst; pp fmt "@]]"

let pp fmt { ty; subty; parameters } =
  pp fmt "{ @[<hov>type = %a/%a;@ parameters = %a@] }"
    pp_ty ty pp_subty subty (pp_lst (fun fmt (key, value) -> pp fmt "%s = %a" key pp_value value)) parameters

let ty { ty; _ } = ty
let subty { subty; _ } = subty
let parameters { parameters; _ } = parameters

let make ?(parameters = []) ty subty =
  (* See RFC 2045 § Appendix A

     content := "Content-Type" ":" type "/" subtype
                *(";" parameter)
                ; Matching of media type and subtype
                ; is ALWAYS case-insensitive.

     parameter := attribute "=" value

     attribute := token
                  ; Matching of attributes
                  ; is ALWAYS case-insensitive.

     See RFC 2045 § 5.1:

     The type, subtype, and parameter names are not case sensitive. For example,
     TEXT,  Text,  and TeXt are all equivalent top-level media types.  Parameter
     values are  normally case  sensitive,  but sometimes  are interpreted  in a
     case-insensitive  fashion,  depending on  the intended  use.  (For example,
     multipart boundaries  are case-sensitive,  but the  "access-type" parameter
     for message/External-body is not case-sensitive.)
  *)
  let parameters =
    List.map (fun (k, v) -> (String.lowercase_ascii k, v)) parameters in
  { ty; subty; parameters; }

(* See RFC 2045 § 5.2 *)
let default =
  { ty = `Text; subty = `Iana_token "plain"
  ; parameters = ["charset", `Token "us-ascii"] }

type field = [ `ContentType of t ]

let field_of_lexer = function
  | `ContentType (ty, subty, parameters ) -> `ContentType { ty; subty; parameters; }

let pp_field fmt = function
  | `ContentType t ->
    Format.fprintf fmt "@[<hov>Content-Type = %a@]" pp t

module D =
struct
  let of_lexer (ty, subty, parameters) p state =
    p { ty; subty; parameters; } state
  let of_lexer' (ty, subty, parameters) =
    { ty; subty; parameters; }
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_ty = function
      | `Application  -> w_string "application"
      | `Audio        -> w_string "audio"
      | `Ietf_token s -> w_string s
      | `Image        -> w_string "image"
      | `Message      -> w_string "message"
      | `Multipart    -> w_string "multipart"
      | `Text         -> w_string "text"
      | `Video        -> w_string "video"
      | `X_token s    -> w_string s

    let w_subty = function
      | `Ietf_token s -> w_string s
      | `Iana_token s -> w_string s
      | `X_token s    -> w_string s

    let w_value = function
      | `String s -> w_char '"' $ Address.E.w_safe_string s $ w_char '"'
      | `Token s  -> w_string s

    let w_parameter (key, value) =
      w_hovbox 1
      $ w_string key
      $ w_close_box
      $ w_char '='
      $ w_hovbox 1
      $ w_value value
      $ w_close_box

    let w_content { ty; subty; parameters; } =
      let w_lst w_sep w_data l =
        let rec aux = function
          | [] -> noop
          | x :: r -> w_sep $ w_hovbox 1 $ w_data x $ w_close_box $ aux r
        in aux l
      in
      w_hovbox 1
      $ w_ty ty
      $ w_close_box
      $ w_char '/'
      $ w_hovbox 1
      $ w_subty subty
      $ w_close_box
      $ w_hovbox 1
      $ w_lst (w_char ';' $ w_space) w_parameter parameters
      $ w_close_box
      $ w_close_box

    let w_crlf k e = w "\r\n" k e

    let w_field = function
      | `ContentType t ->
        w "Content-Type: "
        $ Wrap.lift
        $ Wrap.(fun k -> (w_hovbox (String.length "Content-Type: ")
                          $ w_content t
                          $ w_close_box) (unlift k))
        $ w_crlf
  end

  let w = Internal.w_field
end

let equal = (=)
