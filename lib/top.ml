let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

type field_message =
  [ Rfc5322.field | Rfc2045.field | Rfc2045.field_version | Rfc5322.skip ]
type field_part =
  [ Rfc5322.field | Rfc2045.field | Rfc5322.skip ]

type ('a, 'b) message =
  | Discrete  of MrMime_content.t * field_message list * 'a
  | Extension of MrMime_content.t * field_message list * 'b
  | Multipart of MrMime_content.t * field_message list * (MrMime_content.t * field_part list * ('a, 'b) part option) list
  | Message   of MrMime_content.t * field_message list * MrMime_header.header * ('a, 'b) message
and ('a, 'b) part =
  | PDiscrete  of 'a
  | PExtension of 'b
  | PMultipart of (MrMime_content.t * field_part list * ('a, 'b) part option) list
  | PMessage   of MrMime_header.header * ('a, 'b) message

type encoding = ..
type encoding += Base64 of MrMime_base64.result
type encoding += QuotedPrintable of string
type encoding += Raw of string

type content = ..
type content += Unit

include Parser
include Parser.Convenience

type err += Expected_boundary

let message_headers =
  Rfc5322.header
    (Rfc2045.message_field
       (fun _ -> fail Rfc5322.Nothing_to_do)
       (fun _ -> fail Rfc5322.Nothing_to_do))
  >>= MrMime_header.Decoder.header
  >>= fun (header, rest) -> MrMime_content.Decoder.message rest
  >>= fun (content, rest) -> return (header, content, rest)
  (* Rfc2045.mime_message_headers
   *   (fun _ -> fail Rfc5322.Nothing_to_do) mime-extension
   *   (Rfc5322.field (fun _ -> fail Rfc5322.Nothing_to_do)) *)

let boundary content =
  try List.assoc "boundary" content.MrMime_content.ty.MrMime_contentType.parameters
      |> function `Token s | `String s -> Some s
  with Not_found -> None

let decoder_hashtbl : (string, (unit t -> unit t -> encoding t)) Hashtbl.t = Hashtbl.create 16
let content_hashtbl : (string, (string option -> MrMime_content.t -> field_message list -> content t)) Hashtbl.t = Hashtbl.create 16

let octet boundary content fields =
  let boundary, rollback = match boundary with
    | Some boundary ->
      Rfc2046.delimiter boundary,
      { f = fun i s fail succ ->
        Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) @@  ("\r\n--" ^ boundary));
        succ i s () }
    | None -> return (), return ()
  in

  match content.MrMime_content.encoding with
  | `QuotedPrintable ->
    MrMime_quotedPrintable.decode boundary rollback
    >>| fun v -> QuotedPrintable v
  | `Base64 ->
    MrMime_base64.decode boundary rollback
    >>| fun v -> Base64 v
  | `Bit7 | `Bit8 | `Binary ->
    Rfc5322.decode boundary rollback
    >>| fun v -> Raw v
  | `Ietf_token s | `X_token s ->
    try (Hashtbl.find decoder_hashtbl s) boundary rollback
    with Not_found -> Rfc5322.decode boundary rollback >>| fun v -> Raw v

let discard = function
  | None ->
    let rec loop i s fail succ =
      Input.radvance i (Input.ravailable i);

      let succ' i' s' = succ i' s' () in
      let fail' i' s' = succ i' s' () in

      if s = Complete
      then succ i s ()
      else IO.prompt i fail' succ'
    in

    { f = loop }
  | Some boundary ->
    let boundary, rollback =
      Rfc2046.delimiter boundary,
      { f = fun i s fail succ ->
        Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) @@  ("\r\n--" ^ boundary));
        succ i s () }
    in

    (fix @@ fun m ->
       { f = fun i s fail succ ->
         let _ = Input.transmit i (fun buff off len -> locate buff off len ((<>) '\r')) in
         succ i s () }
       *> ((boundary *> return true)
           <|> (advance 1 *> m)))
    >>= function true -> rollback | false -> return ()

let discard_body = discard None
let discard_part boundary = discard (Some boundary)

let body message =
  let fix' f =
    let rec u a b c = lazy (f r a b c)
    and r a b c = { f = fun i s fail succ ->
              Lazy.(force (u a b c)).f i s fail succ }
    in r
  in

  fix' @@ fun m parent content fields ->
  match content.MrMime_content.ty.MrMime_contentType.ty with
  | `Ietf_token s | `X_token s ->
    (try (Hashtbl.find content_hashtbl s) parent content (fields :> field_message list)
     with exn -> (discard parent *> return Unit))
    >>| fun v -> PExtension v
  | #Rfc2045.discrete  ->
    octet parent content fields
    >>| fun v -> PDiscrete v
  | `Message ->
    message parent
    >>| fun (header', message') -> PMessage (header', message')
  | `Multipart ->
    match boundary content with
    | Some boundary ->
      Rfc2046.multipart_body parent boundary (m (Some boundary))
      >>| fun v -> PMultipart v
    | None -> fail Expected_boundary

let message =
  let fix' f =
    let rec u a = lazy (f r a)
    and r a = { f = fun i s fail succ ->
              Lazy.(force (u a)).f i s fail succ }
    in r
  in

  fix' @@ fun m parent ->
  message_headers
  <* Rfc822.crlf
  >>= fun (header, content, fields) -> match content.MrMime_content.ty.MrMime_contentType.ty with
  | `Ietf_token s | `X_token s ->
    (try (Hashtbl.find content_hashtbl s) None content fields
     with exn -> (discard parent *> return Unit))
    >>| fun v -> header, Extension (content, fields, v)
  | #Rfc2045.discrete  ->
    octet parent content fields
    >>| fun v -> header, Discrete (content, fields, v)
  | `Message ->
    m parent >>| fun (header', message') ->
      header, Message (content, fields, header', message')
  | `Multipart ->
    match boundary content with
    | Some boundary ->
      Rfc2046.multipart_body parent boundary (body m (Some boundary))
      >>| fun v -> header, Multipart (content, fields, v)
    | None -> fail Expected_boundary

let message = message None
