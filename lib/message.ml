type field = Grammar.field

type t = Header.unstrict *
  [ `Composite of Content.t * nest option list
  | `Discrete of Content.t * string ]
and nest =
  [ `Composite of (Content.t * field list) * 'a option list
  | `Discrete of (Content.t * field list) * string ] as 'a

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_option pp_data fmt = function
  | Some data -> pp_data fmt data
  | None -> pp fmt "<none>"

let pp_field : Format.formatter -> field -> unit =
  fun fmt -> function
  | #Header.field as x -> Header.pp_field fmt x
  | #Content.Part.field as x -> Content.Part.pp_field fmt x

let rec pp_nest : Format.formatter -> nest -> unit =
  fun fmt -> function
  | `Composite ((content, fields), lst) ->
      pp fmt "{ @[<v>content = %a;@\nheader = @[<v>%a@];@\n%a@] }"
        Content.pp content
        (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_field) fields
        (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") (pp_option pp_nest)) lst
  | `Discrete ((content, fields), body) ->
      pp fmt "{ @[<v>content = %a;@\nheader = @[<v>%a@];@\nbody = %S@] }"
        Content.pp content
        (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_field) fields
        body

let pp fmt (header, top) = match top with
  | `Composite (content, body) ->
    pp fmt "{ @[<v>header = %a;@\ncontent = %a;@\n%a@] }"
      Header.pp header Content.pp content (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") (pp_option pp_nest)) body
  | `Discrete (content, body) ->
    pp fmt "{ @[<v>header = %a;@\ncontent = %a;@\nbody = %S@] }"
      Header.pp header Content.pp content body

module D =
struct
  open BaseDecoder

  let of_decoder state =
    let rec loop = function
      | `Error (err, buf, off, len) ->
        raise (Error.Error (`Error (err, buf, off, len)))
      | `Read (buf, off, len, k) ->
        loop @@ (k 0)
      | `Ok data -> data
    in

    let rule =
      Grammar.p_message
      @ fun header message state -> `Ok (header, message)
    in

    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder

    let w_encode body =
      let len = String.length body in

      let rec aux idx =
        if idx < len
        then match String.get body idx with
             (* Unix newline encoder *)
             | '\n' when Newline.is_lf ->
               w "\r\n" $ aux (idx + 1)
             (* Windows newline encoder *)
             | '\r' when Newline.is_crlf && (idx + 1 < len) && String.get body (idx + 1) = '\n' ->
               w "\r\n" $ aux (idx + 2)
             | chr  -> w_char chr $ aux (idx + 1)
        else noop
      in

      aux 0

    let w_body content body = match Content.encoding content with
      | `Bit8
      | `Ietf_token _
      | `X_token _
      | `Bit7
      | `Binary -> w_encode body
      | `Base64 -> Base64.w_encode body
      | `QuotedPrintable -> QuotedPrintable.w_encode body

    let w_crlf k e = w "\r\n" k e

    let w_field (field : field) = match field with
      | #Content.Part.field as x -> Content.Part.E.w_field x
      | #Header.field as x -> Header.E.w_field x

    let w_fields fields = List.fold_right w_field fields

    let rec w_multipart content lst =
      let boundary = Rfc2045.value_to_string @@ List.assoc "boundary" (ContentType.parameters @@ Content.ty content) in
      let rec aux = function
        | [] | [ None ]-> w (Rfc2046.m_close_delimiter boundary)
        | [ Some (`Discrete ((content, fields), body)) ] ->
          Content.Part.E.w (Content.Part.to_field content)
          $ w_fields fields
          $ w_crlf
          $ w_body content body
          $ w (Rfc2046.m_close_delimiter boundary)
        | [ Some (`Composite ((content, fields), lst)) ] ->
          Content.Part.E.w (Content.Part.to_field content)
          $ w_fields fields
          $ w_multipart content lst
          $ w (Rfc2046.m_close_delimiter boundary)
        | Some (`Discrete ((content, fields), body)) :: rest ->
          Content.Part.E.w (Content.Part.to_field content)
          $ w_fields fields
          $ w_crlf
          $ w_body content body
          $ w (Rfc2046.m_delimiter boundary)
          $ w_crlf $ aux rest
        | Some (`Composite ((content, fields), lst)) :: rest ->
          Content.Part.E.w (Content.Part.to_field content)
          $ w_fields fields
          $ w_multipart content lst
          $ w (Rfc2046.m_delimiter boundary)
          $ w_crlf $ aux rest
        | None :: rest ->
          w (Rfc2046.m_delimiter boundary) $ w_crlf $ aux rest
      in
      w (Rfc2046.m_delimiter boundary)
      $ w_crlf
      $ aux lst

    let w_message (header, body) =
      Header.E.w (Header.to_field header)
      $ match body with
        | `Composite (content, lst) ->
          Content.Message.E.w (Content.Message.to_field content)
          $ w_multipart content lst
        | `Discrete (content, body) ->
          Content.Message.E.w (Content.Message.to_field content)
          $ w_crlf
          $ w_body content body
  end

  let w = Internal.w_message

  let to_buffer t state =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> buf
    in

    let rule =
      let open BaseEncoder in
      let ok = flush (fun _ -> `Ok) in
      Internal.w_message t ok
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string s)
let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

let equal = (=)
