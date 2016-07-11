type field_message =
  [ Rfc5322.field | Rfc2045.field | Rfc2045.field_version | Rfc5322.skip ]
type field_part =
  [ Rfc5322.field | Rfc2045.field | Rfc5322.skip ]

type 'a message =
  | Discrete  of Content.t * field_message list * 'a
  | Extension of Content.t * field_message list
  | Composite of Content.t * field_message list * (Content.t * field_part list * 'a part option) list
and 'a part =
  | PDiscrete  of Content.t * field_part list * 'a
  | PExtension of Content.t * field_part list
  | PComposite of Content.t * field_part list * (Content.t * field_part list * 'a part option) list

type content = ..

include Parser
include Parser.Convenience

type err += Expected_boundary

let message_headers =
  Rfc5322.header
    (Rfc2045.message_field
       (fun _ -> fail Rfc5322.Nothing_to_do)
       (fun _ -> fail Rfc5322.Nothing_to_do))
  >>= Header.decoder >>= fun (header, rest) -> Content.message rest
  >>= fun (content, rest) -> return (header, content, rest)
  (* Rfc2045.mime_message_headers
   *   (fun _ -> fail Rfc5322.Nothing_to_do) mime-extension
   *   (Rfc5322.field (fun _ -> fail Rfc5322.Nothing_to_do)) *)

let boundary content =
  try List.assoc "boundary" content.Content.ty.ContentType.parameters
      |> function `Token s | `String s -> Some s
  with Not_found -> None

type content += Base64 of Base64.result
type content += QuotedPrintable of string
type content += Raw of string

let octet boundary content fields =
  let boundary, rollback = match boundary with
    | Some boundary ->
      Rfc2046.delimiter boundary,
      { f = fun i s fail succ ->
        Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) @@  ("\r\n--" ^ boundary));
        succ i s () }
    | None -> return (), return ()
  in

  match content.Content.encoding with
  | `QuotedPrintable ->
    QuotedPrintable.decode boundary rollback
    >>| fun v -> QuotedPrintable v
  | `Base64 ->
    Base64.decode boundary rollback
    >>| fun v -> Base64 v
  | _ ->
    Rfc5322.decode boundary rollback
    >>| fun v -> Raw v

let body =
  let fix' f =
    let rec u a b c = lazy (f r a b c)
    and r a b c = { f = fun i s fail succ ->
              Lazy.(force (u a b c)).f i s fail succ }
    in r
  in

  fix' @@ fun m parent content fields ->
  match content.Content.ty.ContentType.ty with
  | #Rfc2045.extension -> return (PExtension (content, fields))
  | #Rfc2045.discrete  ->
    octet parent content fields
    >>| fun v -> PDiscrete (content, fields, v)
  | #Rfc2045.composite ->
    match boundary content with
    | Some boundary ->
      Rfc2046.multipart_body parent boundary (m (Some boundary))
      >>| fun v -> PComposite (content, fields, v)
    | None -> fail Expected_boundary

let message =
  message_headers
  <* Rfc822.crlf
  >>= fun (header, content, fields) -> match content.Content.ty.ContentType.ty with
  | #Rfc2045.extension -> return (header, Extension (content, fields))
  | #Rfc2045.discrete  ->
    octet None content fields
    >>| fun v -> header, Discrete (content, fields, v)
  | #Rfc2045.composite ->
    match boundary content with
    | Some boundary ->
      Rfc2046.multipart_body None boundary (body (Some boundary))
      >>| fun v -> header, Composite (content, fields, v)
    | None -> fail Expected_boundary
