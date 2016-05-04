type t =
  { ty       : ContentType.t
  ; encoding : ContentEncoding.t
  ; version  : MimeVersion.t
  ; id       : MsgID.t option }

let ty { ty; _ } = ty
let encoding { encoding; _ } = encoding

let make
  ?(ty = ContentType.default)
  ?(encoding = ContentEncoding.default)
  ?(version = MimeVersion.default)
  ?id () =
  { ty; encoding; version; id; }

let of_lexer fields p state =
  let ty       = ref None in
  let encoding = ref None in
  let version  = ref None in
  let id       = ref None in

  let sanitize fields =
    p ({ ty       = Option.value ~default:ContentType.default !ty
       ; encoding = Option.value ~default:ContentEncoding.default !encoding
       ; version  = Option.value ~default:MimeVersion.default !version
       ; id       = !id }) fields state
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
      | `MimeVersion v ->
        (match !version with
         | None   -> version := Some (MimeVersion.of_lexer v);
                     loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ContentID e ->
        (match !id with
         | None   -> id := Some (MsgID.of_lexer e);
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
  p fmt "Content-Encoding: %a\r\n" ContentEncoding.pp encoding;
  pp_field_opt fmt "Content-ID" MsgID.pp id;
