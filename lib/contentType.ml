open Base

type ty    = Rfc2045.ty
type subty = Rfc2045.subty
type value = Rfc2045.value

type t =
  { ty         : ty
  ; subty      : subty
  ; parameters : (string * value) list }

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
    List.map (fun (k, v) -> (String.lowercase k, v)) parameters in
  { ty; subty; parameters; }

(* See RFC 2045 § 5.2 *)
let default =
  { ty = `Text; subty = `Iana_token "plain"
  ; parameters = ["charset", `Token "us-ascii"] }

let of_lexer (ty, subty, parameters) =
  { ty; subty; parameters; }

let p = Format.fprintf

let pp_ty fmt = function
  | `Application  -> Format.pp_print_string fmt "application"
  | `Audio        -> Format.pp_print_string fmt "audio"
  | `Ietf_token s -> p fmt "%s" s
  | `Image        -> Format.pp_print_string fmt "image"
  | `Message      -> Format.pp_print_string fmt "message"
  | `Multipart    -> Format.pp_print_string fmt "multipart"
  | `Text         -> Format.pp_print_string fmt "text"
  | `Video        -> Format.pp_print_string fmt "video"
  | `X_token s    -> p fmt "%s" s

let pp_subty fmt = function
  | `Ietf_token s -> p fmt "%s" s
  | `Iana_token s -> p fmt "%s" s
  | `X_token s    -> p fmt "%s" s

let pp_value fmt = function
  | `String s -> p fmt "\"%a\"" (pp_string ~in_qs:true ~in_dm:false) s
  | `Token s  -> Format.pp_print_string fmt s

let pp fmt { ty; subty; parameters; } =
  let rec aux fmt = function
    | [] -> ()
    | (attribute, value) :: r ->
      Format.fprintf fmt "; %s=%a" attribute pp_value value;
      aux fmt r
  in
  Format.fprintf fmt "%a/%a%a"
    pp_ty ty pp_subty subty aux parameters

let equal = (=)
