type t =
  { ty       : ContentType.t
  ; encoding : ContentEncoding.t
  ; version  : MimeVersion.t
  ; id       : MsgID.t option }

let make
  ?(ty = ContentType.default)
  ?(encoding = ContentEncoding.default)
  ?(version = MimeVersion.default)
  ?id () =
  { ty; encoding; version; id; }

let of_lexer k l =
  let ty       = ref None in
  let encoding = ref None in
  let version  = ref None in
  let id       = ref None in

  let sanitize fields =
    k ({ ty       = Option.value ~default:ContentType.default !ty
       ; encoding = Option.value ~default:ContentEncoding.default !encoding
       ; version  = Option.value ~default:MimeVersion.default !version
       ; id       = !id }) fields
  in

  let rec loop i l = match l with
    | [] -> sanitize (List.rev i)
    | x :: rest ->
      match x with
      | `ContentType c ->
        (match !ty with
         | None   -> ty := Some (ContentType.of_lexer c);
                     loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ContentEncoding e ->
        (match !encoding with
         | None   -> encoding := Some (ContentEncoding.of_lexer e);
                     loop i rest
         | Some _ -> loop (x :: i) rest)
      | `MimeVersion v ->
        (match !version with
         | None   -> version := Some (MimeVersion.of_lexer v);
                     loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ContentID e ->
        (match !id with
         | None   -> id := Some (MsgID.of_lexer e);
                     loop i rest
         | Some _ -> loop (x :: i) rest)
      | field -> loop (field :: i) rest
  in

  loop [] l

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
