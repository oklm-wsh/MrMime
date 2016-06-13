type t =
  { ty          : ContentType.t
  ; encoding    : ContentEncoding.t
  ; version     : MimeVersion.t
  ; id          : MsgID.t option
  ; description : string option
  ; content     : (string * Rfc5322.phrase) list }

let ty { ty; _ } = ty
let encoding { encoding; _ } = encoding

let make
  ?(ty = ContentType.default)
  ?(encoding = ContentEncoding.default)
  ?(version = MimeVersion.default)
  ?id ?description
  ?(extension = []) () =
  { ty; encoding; version; id; description; content = extension; }

module Part =
struct
  type field =
    [ ContentType.field
    | ContentEncoding.field
    | `ContentID          of MsgID.t
    | `ContentDescription of string
    | `Content            of string * Rfc5322.phrase
    | `Unsafe             of string * Rfc5322.phrase ]

  let field_of_lexer = function
    | `Content p                -> `Content p
    | `ContentDescription s     -> `ContentDescription s
    | (`ContentType _) as x     -> (ContentType.field_of_lexer x :> field)
    | (`ContentEncoding _) as x -> (ContentEncoding.field_of_lexer x :> field)
    | `ContentID i              -> `ContentID (MsgID.D.of_lexer i)
    | `Unsafe (field, value)    -> `Unsafe (field, value)

  let to_field t : field list =
    let ( >>= ) o f = match o with Some x -> Some (f x) | None -> None in
    let ( @:@ ) o r = match o with Some x -> x :: r | None -> r in

    (t.id >>= fun m -> `ContentID m)
    @:@ (t.description >>= fun s -> `ContentDescription s)
    @:@ (`ContentType t.ty) :: (`ContentEncoding t.encoding) :: []
    @ (List.map (fun (key, value) -> `Content (key, value)) t.content)
    @ (List.map (fun (key, value) -> `Unsafe (key, value)) [])

  module D =
  struct
    let of_lexer' extend fields p state =
      [%debug Printf.printf "state: of_lexer (content)\n%!"];

      let ty          = ref None in
      let encoding    = ref None in
      let id          = ref None in
      let description = ref None in
      let content     = ref [] in

      let sanitize fields =
        p ({ ty          = Option.value ~default:ContentType.default !ty
           ; encoding    = Option.value ~default:ContentEncoding.default !encoding
           ; version     = MimeVersion.default
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
             | None   -> ty := Some (ContentType.D.of_lexer' c);
                         loop garbage rest
             | Some _ -> loop (field :: garbage) rest)
          | `ContentEncoding e ->
            (match !encoding with
             | None   -> encoding := Some (ContentEncoding.D.of_lexer' e);
                         loop garbage rest
             | Some _ -> loop (field :: garbage) rest)
          | `ContentID e ->
            (match !id with
             | None   -> id := Some (MsgID.D.of_lexer e);
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
          | field -> extend loop field garbage rest
      in

      loop [] fields

    let of_lexer fields p state =
      of_lexer' (fun self field garbage rest -> self (field :: garbage) rest) fields p state
  end

  module E =
  struct
    module Internal =
    struct
      open BaseEncoder
      open Wrap

      let w_crlf k e = w "\r\n" k e

      let w_content = function
        | #ContentType.field as x -> ContentType.E.w x
        | #ContentEncoding.field as x -> ContentEncoding.E.w x
        | `ContentID m ->
          w "Content-ID: "
          $ Wrap.lift
          $ Wrap.(fun k -> (w_hovbox (String.length "Content-ID: ")
                            $ MsgID.E.w m
                            $ w_close_box) (unlift k))
          $ w_crlf
        | `ContentDescription s ->
          w "Content-Description: "
          $ Wrap.lift
          $ Wrap.(fun k -> (w_hovbox (String.length "Content-Description: ")
                            $ w_string s
                            $ w_close_box) (unlift k))
          $ w_crlf
        | `Content (field, value) ->
          w ("Content-" ^ field) $ w ":"
          $ Wrap.lift
          $ Wrap.(fun k -> (w_hovbox (String.length field + 10)
                            $ Address.E.w_phrase value
                            $ w_close_box) (unlift k))
          $ w_crlf
        | `Unsafe (key, value) ->
          w key $ w ":"
          $ Wrap.lift
          $ Wrap.(fun k -> (w_hovbox (String.length key + 2)
                            $ Address.E.w_phrase value
                            $ w_close_box) (unlift k))
          $ w_crlf
    end

    let w_field = Internal.w_content
    let w fields = List.fold_right Internal.w_content fields
  end
end

module Message =
struct
  type field = [ Part.field | MimeVersion.field ]

  let field_of_lexer = function
    | #Rfc2045.field as x -> (Part.field_of_lexer x :> field)
    | #Rfc2045.mime_field as x -> (MimeVersion.field_of_lexer x :> field)

  let to_field t =
    (`MimeVersion t.version :> field) :: (Part.to_field t :> field list)

  module D =
  struct
    let of_lexer fields p =
      let version = ref None in

      Part.D.of_lexer'
        (fun self field garbage rest ->
         match field with
         | `MimeVersion x ->
           (match !version with
            | None -> version := Some (MimeVersion.D.of_lexer' x);
                      self garbage rest
            | Some _ -> self (field :: garbage) rest)
         | field -> self (field :: garbage) rest)
        fields
        (fun content -> p { content with version = Option.value !version ~default:MimeVersion.default })
  end

  module E =
  struct
    module Internal =
    struct
      let w_content = function
        | #Part.field as x -> Part.E.w_field x
        | #MimeVersion.field as x -> MimeVersion.E.w x
    end

    let w_field = Internal.w_content
    let w fields = List.fold_right Internal.w_content fields
  end
end
