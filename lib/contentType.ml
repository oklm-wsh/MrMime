type t =
  {
    ty         : string;
    subty      : string;
    parameters : (string * string) list;
  }

let ty { ty; _ } = ty
let subty { subty; _ } = subty
let parameters { parameters; _ } = parameters

let make ?(parameters = []) ty subty =
  (** See RFC 2045 § Appendix A

      content := "Content-Type" ":" type "/" subtype
                 *(";" parameter)
                 ; Matching of media type and subtype
                 ; is ALWAYS case-insensitive.

      parameter := attribute "=" value

      attribute := token
                   ; Matching of attributes
                   ; is ALWAYS case-insensitive.

      See RFC 2045 § 5.1:

      The  type,  subtype,  and parameter  names  are  not  case sensitive.  For
      example,  TEXT,  Text,  and TeXt are all equivalent top-level media types.
      Parameter  values   are  normally  case   sensitive,   but  sometimes  are
      interpreted in a case-insensitive fashion,  depending on the intended use.
      (For  example,   multipart   boundaries   are   case-sensitive,   but  the
      "access-type" parameter for message/External-body is not case-sensitive.)
  *)
  let ty = String.lowercase ty in
  let subty = String.lowercase subty in
  let parameters =
    List.map (fun (k, v) -> (String.lowercase k, v)) parameters in
  { ty; subty; parameters; }

(** See RFC 2045 § 5.2 *)
let default =
  { ty = "text"; subty = "plain";
    parameters = ["charset", "us-ascii"]; }

let pp fmt { ty; subty; parameters; } =
  let rec aux fmt = function
    | [] -> ()
    | (attribute, value) :: r ->
      Format.fprintf fmt "; %s=%s" attribute value;
      aux fmt r
  in
  Format.fprintf fmt "%s/%s%a"
    ty subty aux parameters

let equal = (=)
