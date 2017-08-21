type field_message = Top.field_message
type field_part    = Top.field_part

type ('a, 'b) message = ('a, 'b) Top.message =
  | Discrete  of Content.t * field_message list * 'a
  | Extension of Content.t * field_message list * 'b
  | Multipart of Content.t * field_message list * (Content.t * field_part list * ('a, 'b) part option) list
  | Message   of Content.t * field_message list * Header.header * ('a, 'b) message
and ('a, 'b) part = ('a, 'b) Top.part =
  | PDiscrete  of 'a
  | PExtension of 'b
  | PMultipart of (Content.t * field_part list * ('a, 'b) part option) list
  | PMessage   of Header.header * ('a, 'b) message

type encoding = Top.encoding = ..
type Top.encoding += Base64 = Top.Base64
type Top.encoding += QuotedPrintable = Top.QuotedPrintable
type Top.encoding += Raw = Top.Raw

type content = Top.content = ..
type Top.content += Unit = Top.Unit

module Decoder =
struct
  open Parser
  open Parser.Convenience

  let p_message = Top.message

  type without_skip =
    [ Rfc5322.field_header
    | Rfc2045.field
    | Rfc2045.field_version
    | Rfc5322.resent
    | Rfc5322.trace ]
  type skipped = (without_skip * string list) option
  type result =
    [ `Skip
    | `Continue of skipped
    | `Field   of (skipped * without_skip) ]

  let fix_skipped base skipped =
    let buffer = Buffer.create 16 in
    let state  = Encoder.make () in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buffer s i l;
        loop @@ (k l)
      | `Ok ->
        Buffer.add_char buffer ' ';
        Buffer.add_string buffer (String.concat "\r\n " skipped); (* FWS *)
        Buffer.add_string buffer "\r\n";

        Buffer.contents buffer
    in

    let rule = match base with
      | #Rfc5322.field_header as x  -> Header.Encoder.w_field x
      | #Rfc2045.field as x         -> Content.Encoder.w_field x
      | #Rfc2045.field_version as x -> MimeVersion.Encoder.w_field x
      | #Rfc5322.resent as x        -> Resent.Encoder.w_field x
      | #Rfc5322.trace as x         -> Trace.Encoder.w_field x
    in

    loop @@ rule (Encoder.flush (fun _ -> `Ok)) state

  let _field ~skipped : result Parser.t =
    let rule =
      (Rfc5322.field_name
       <* (many (satisfy (function '\x09' | '\x20' -> true | _ -> false)))
       <* char ':'
       >>= fun field_name -> Rfc5322.field (Rfc2045.message_field
                                            (fun _ -> fail Rfc5322.Nothing_to_do)
                                            (fun _ -> fail Rfc5322.Nothing_to_do))
                                           (Bytes.to_string field_name))
    in
    rule >>= fun field -> match skipped, field with
      | None, #Rfc5322.skip -> return `Skip
      | None, (#without_skip as e) ->
        return (`Continue (Some (e, [])))
      | Some (base, skipped), (#Rfc5322.skip as x) ->
        let `Skip line = x in
        return (`Continue (Some (base, line :: skipped)))
      | Some (base, skipped), (#without_skip as e) ->
        if List.length skipped = 0
        then return (`Field (Some (e, []), base))
        else begin
          let fixed = fix_skipped base skipped in
          let local_input = Input.create_bytes (String.length fixed) in

          Input.write_string local_input fixed 0 (String.length fixed);

          (* last chance *)
          let to_result = function
            | Parser.Fail _
            | Parser.Read _ -> return (`Field (Some (e, []), base))
            | Parser.Done #Rfc5322.skip -> assert false
            | Parser.Done (#without_skip as v) -> return (`Field (Some (e, []), v))
          in

          to_result @@ Parser.only local_input rule
        end

  let p_header =
    Rfc5322.header
      (Rfc2045.message_field
        (fun _ -> fail Rfc5322.Nothing_to_do)
        (fun _ -> fail Rfc5322.Nothing_to_do))
    >>= Header.Decoder.header
    >>= fun (header, rest) -> Content.Decoder.message rest
    >>= fun (content, rest) -> return (header, content, rest)

  let p_first_part content =
    match Top.boundary content with
    | Some boundary ->
      option () (Rfc2046.preamble boundary)
      *> Rfc2046.dash_boundary boundary
      *> Rfc2046.transport_padding
      *> Rfc822.crlf
      *> Rfc2045.mime_part_headers
           (Rfc5322.field (fun _ -> fail Rfc5322.Nothing_to_do))
      >>= Content.Decoder.part
    | None -> fail Top.Expected_boundary

  let p_next_part content =
    match Top.boundary content with
    | Some boundary ->
      Rfc2046.delimiter boundary
      *> Rfc2046.transport_padding
      *> Rfc822.crlf
      *> Rfc2045.mime_part_headers
           (Rfc5322.field (fun _ -> fail Rfc5322.Nothing_to_do))
      >>= Content.Decoder.part
    | None -> fail Top.Expected_boundary

  let p_bound_of_content content =
    match Top.boundary content with
    | Some boundary ->
      Rfc2046.delimiter boundary,
      { Parser.f = fun i s _fail succ ->
        Parser.Input.rollback i
          (Internal_buffer.from_string ~proof:(Input.proof i)
           @@ ("\r\n--" ^ boundary));

        succ i s () }
    | None -> Parser.return (), Parser.return ()

  let p_end_of_part parent_content =
    match Top.boundary parent_content with
    | Some boundary ->
      ((Rfc2046.close_delimiter boundary *> return `End)
       <|> return `Next)
    | None -> fail Top.Expected_boundary

  let p_store_part parent_content current_content =
    let open Parser in
    let open Parser.Convenience in
    match Top.boundary parent_content with
    | Some boundary ->
      let boundary', rollback = p_bound_of_content parent_content in

      let decoder = match current_content.Content.encoding with
        | `QuotedPrintable ->
          QuotedPrintable.Decoder.decode boundary' rollback
          >>| fun v -> Top.QuotedPrintable v
        | `Base64 ->
          Base64.Decoder.decode boundary' rollback
          >>| fun v -> Top.Base64 v
        | _ ->
          Rfc5322.decode boundary' rollback
          >>| fun v -> Top.Raw (Bytes.to_string v)
      in

      option None (Rfc822.crlf *> decoder >>| fun v -> Some v)
      >>= fun part ->
        ((Rfc2046.close_delimiter boundary *> return (`End part)) <|> return (`Next part))
    | None -> fail Top.Expected_boundary

  let p_discard_part parent_content =
    let open Parser in
    match Top.boundary parent_content with
    | Some boundary ->
      Top.discard_part boundary
      *> ((Rfc2046.close_delimiter boundary *> return `End) <|> return `Next)
    | None -> fail Top.Expected_boundary
end

module Encoder =
struct
  open Encoder

  let w_encode body =
    let len = String.length body in

    let rec aux idx =
      if idx < len
      then match String.get body idx with
           (* Unix newline encoder *)
           | '\n' when true ->
             string "\r\n" $ aux (idx + 1)
           (* Windows newline encoder *)
           | '\r' when false && (idx + 1 < len) && String.get body (idx + 1) = '\n' ->
             string "\r\n" $ aux (idx + 2)
           | chr  -> char chr $ aux (idx + 1)
      else noop
    in

    aux 0

  let w_body content body = match content.Content.encoding, body with
    | `Bit8           , Top.Raw body
    | `Ietf_token _   , Top.Raw body
    | `X_token _      , Top.Raw body
    | `Bit7           , Top.Raw body
    | `Binary         , Top.Raw body             -> w_encode body
    | `Base64         , Top.Base64 (`Dirty body) -> Base64.Encoder.w_encode body
    | `Base64         , Top.Base64 (`Clean body) -> Base64.Encoder.w_encode body
    | `QuotedPrintable, Top.QuotedPrintable body -> QuotedPrintable.Encoder.w_encode body
    | _ -> assert false

  let w_crlf k e = string "\r\n" k e

  exception Expected_boundary

  let rec w_multipart content lst =
    let boundary = match Top.boundary content with
      | Some v -> v
      | None -> raise Expected_boundary
    in
    let rec aux = function
      | [ (content, _fields, Some (Top.PDiscrete body)) ] ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_body content body
      | [ (content, _fields, Some (Top.PMultipart lst)) ] ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_multipart content lst
      | [ (content, _fields, None) ] ->
        Content.Encoder.w_part content
        $ w_crlf
      | (content, _fields, Some (Top.PDiscrete body)) :: r ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_body content body
        $ string (Rfc2046.make_delimiter boundary)
        $ aux r
      | (content, _fields, Some (Top.PMultipart lst)) :: r ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_multipart content lst
        $ string (Rfc2046.make_delimiter boundary)
        $ aux r
      | (content, _fields, None) :: r ->
        Content.Encoder.w_part content
        $ w_crlf
        $ string (Rfc2046.make_delimiter boundary)
        $ aux r
      | _ -> assert false (* impossible to have an empty list *)
                          (* other case, TODO! *)
    in
    string (Rfc2046.make_delimiter boundary)
    $ w_crlf
    $ aux lst

  let _w_message (_header, body) =
    match body with
    | Top.Multipart (content, _fields, lst) ->
      Content.Encoder.w_message content
      $ w_crlf
      $ w_multipart content lst
    | Top.Discrete (content, _fields, body) ->
      Content.Encoder.w_message content
      $ w_crlf
      $ w_body content body
    | _ -> assert false (* TODO: not implemented yet *)
end

let of_string_raw ?(chunk = 1024) s off len =
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (len - (consumed - off)) in
      Input.write_string buffer s consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some (v, consumed - off)
  in

  aux off @@ Parser.run i Top.message

module Extension =
struct
  let respect f s =
    let l = String.length s in
    let i = ref 0 in

    while !i < l && f (String.get s !i) do incr i done;

    !i = l

  let add_encoding token encoding =
    if respect Rfc2045.is_token token
    then Hashtbl.add Top.decoder_hashtbl token encoding
    else raise (Invalid_argument "Message.Extension.add_encoding: bad token")

  let add_content token content =
    if respect Rfc2045.is_token token
    then Hashtbl.add Top.content_hashtbl token content
    else raise (Invalid_argument "Message.Extension.add_content: bad token")
end
