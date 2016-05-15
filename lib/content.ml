type t =
  { ty          : ContentType.t
  ; encoding    : ContentEncoding.t
  ; version     : MimeVersion.t
  ; id          : MsgID.t option
  ; description : string option
  ; content     : (string * string) list }

type field =
  [ `ContentType of ContentType.t
  | `ContentEncoding of ContentEncoding.t
  | `ContentID of MsgID.t
  | `ContentDescription of string
  | `Content of (string * string) ]

let field_of_lexer : Rfc2045.field -> field = function
  | `Content p -> `Content p
  | `ContentDescription s -> `ContentDescription s
  | `ContentType t -> `ContentType (ContentType.of_lexer t)
  | `ContentEncoding e -> `ContentEncoding (ContentEncoding.of_lexer e)
  | `ContentID i -> `ContentID (MsgID.of_lexer i)

let ty { ty; _ } = ty
let encoding { encoding; _ } = encoding

let make
  ?(ty = ContentType.default)
  ?(encoding = ContentEncoding.default)
  ?(version = MimeVersion.default)
  ?id ?description
  ?(extension = []) () =
  { ty; encoding; version; id; description; content = extension; }

let of_lexer fields p state =
  let ty          = ref None in
  let encoding    = ref None in
  let version     = ref None in
  let id          = ref None in
  let description = ref None in
  let content     = ref [] in

  let sanitize fields =
    p ({ ty          = Option.value ~default:ContentType.default !ty
       ; encoding    = Option.value ~default:ContentEncoding.default !encoding
       ; version     = Option.value ~default:MimeVersion.default !version
       ; id          = !id
       ; description = !description
       ; content     = !content }) fields state
  in

  let rec loop garbage fields = match fields with
    | [] -> sanitize (List.rev garbage)
    | field :: rest ->
      match field with
      | `ContentType c ->
        (match !ty with
         | None   -> ty := Some (ContentType.of_lexer c);
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ContentEncoding e ->
        (match !encoding with
         | None   -> encoding := Some (ContentEncoding.of_lexer e);
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ContentID e ->
        (match !id with
         | None   -> id := Some (MsgID.of_lexer e);
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ContentDescription s ->
        (match !description with
         | None   -> description := Some s;
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `Content (field, value) ->
        content := (field, value) :: !content;
        loop garbage rest
      | #Rfc2045.mime_field as v ->
        (match !version with
         | None   -> version := Some (MimeVersion.of_lexer v);
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | field -> loop (field :: garbage) rest
  in

  loop [] fields

let p = Format.fprintf

let pp fmt { ty; encoding; version; id; } =
  let pp_field_opt fmt field_name pp_field field_opt =
    match field_opt with
    | Some field -> p fmt "%s: %a\r\n" field_name pp_field field
    | None       -> ()
  in

  p fmt "MIME-Version: %a\r\n" MimeVersion.pp version;
  p fmt "Content-Type: %a\r\n" ContentType.pp ty;
  p fmt "Content-Transfer-Encoding: %a\r\n" ContentEncoding.pp encoding;
  pp_field_opt fmt "Content-ID" MsgID.pp id;
