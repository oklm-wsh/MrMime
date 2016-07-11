type field_message = Top.field_message
type field_part = Top.field_part

type 'a message = 'a Top.message =
  | Discrete  of Content.t * field_message list * 'a
  | Extension of Content.t * field_message list
  | Composite of Content.t * field_message list * (Content.t * field_part list * 'a part option) list
and 'a part = 'a Top.part =
  | PDiscrete  of Content.t * field_part list * 'a
  | PExtension of Content.t * field_part list
  | PComposite of Content.t * field_part list * (Content.t * field_part list * 'a part option) list

type content = Top.content = ..

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

  let w_body content body = match content.Content.encoding with
    | `Bit8
    | `Ietf_token _
    | `X_token _
    | `Bit7
    | `Binary -> w_encode body
    | `Base64 -> Base64.w_encode body
    | `QuotedPrintable -> QuotedPrintable.w_encode body

  let w_crlf k e = string "\r\n" k e

  exception Expected_boundary

  let rec w_multipart content lst =
    let boundary = match Top.boundary content with
      | Some v -> v
      | None -> raise Expected_boundary
    in
    let rec aux = function
      | [ (content, fields, Some (Top.PDiscrete (_, _, body))) ] ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_body content body
      | [ (content, fields, Some (Top.PComposite (_, _, lst))) ] ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_multipart content lst
      | [ (content, fields, None) ] ->
        Content.Encoder.w_part content
        $ w_crlf
      | (content, fields, Some (Top.PDiscrete (_, _, body))) :: r ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_body content body
        $ string (Rfc2046.make_delimiter boundary)
        $ aux r
      | (content, fields, Some (Top.PComposite (_, _, lst))) :: r ->
        Content.Encoder.w_part content
        $ w_crlf
        $ w_multipart content lst
        $ string (Rfc2046.make_delimiter boundary)
        $ aux r
      | (content, fields, None) :: r ->
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

  let w_message (header, body) =
    match body with
    | Top.Composite (content, fields, lst) ->
      Content.Encoder.w_message content
      $ w_crlf
      $ w_multipart content lst
    | Top.Discrete (content, fields, body) ->
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
