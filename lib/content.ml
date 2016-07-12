module Map = Map.Make(String)

type unstructured = Rfc5322.unstructured
type field        = [ Rfc2045.field | Rfc2045.field_version | Rfc2045.skip ]

type t =
  { ty          : ContentType.content
  ; encoding    : ContentEncoding.mechanism
  ; version     : MimeVersion.version
  ; id          : MsgID.msg_id option
  ; description : unstructured option
  ; content     : unstructured list Map.t
  ; unsafe      : unstructured list Map.t
  ; skip        : string list }

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_option ?(none = (fun fmt () -> pp fmt "<none>")) pp_data fmt = function
  | None -> none fmt ()
  | Some v -> pp_data fmt v

let pp_raw fmt = function
  | Rfc2047.QuotedPrintable raw -> pp fmt "quoted-printable:%s" raw
  | Rfc2047.Base64 (`Clean raw) -> pp fmt "base64:%s" raw
  | Rfc2047.Base64 (`Dirty raw) -> pp fmt "base64:%S" raw
  | Rfc2047.Base64 `Wrong_padding -> pp fmt "base64:wrong-padding"

let pp_unstructured fmt lst =
  let rec aux fmt = function
    | `Text s -> pp fmt "%s" s
    | `WSP    -> pp fmt "@ "
    | `CR i   -> pp fmt "<cr %d>" i
    | `LF i   -> pp fmt "<lf %d>" i
    | `CRLF   -> pp fmt "<crlf>@\n"
    | `Encoded (charset, raw) ->
      pp fmt "{ @[<hov>charset = %s;@ raw = %a@] }"
        charset pp_raw raw
  in
  pp fmt "@[<hov>%a@]"
    (pp_lst ~sep:(fun fmt () -> pp fmt "@,") aux) lst

let pp_field fmt = function
  | `Content (k, v)       -> pp fmt "@[<hov>%s = %a@]" (String.capitalize_ascii k) pp_unstructured v
  | `ContentDescription v -> pp fmt "@[<hov>Content-Description = %a@]" pp_unstructured v
  | `ContentType v        -> pp fmt "@[<hov>Content-Type = %a@]" ContentType.pp v
  | `ContentEncoding v    -> pp fmt "@[<hov>Content-Encoding = %a@]" ContentEncoding.pp v
  | `ContentID v          -> pp fmt "@[<hov>Content-ID = %a@]" MsgID.pp v
  | `MimeVersion v        -> pp fmt "@[<hov>MIME-Version = %a@]" MimeVersion.pp v
  | `Unsafe (k, v)        -> pp fmt "@[<hov>%s # %a@]" (String.capitalize_ascii k) pp_unstructured v
  | `Skip line            -> pp fmt "@[<hov># %S@]" line

let pp fmt { ty; encoding; version; id; description; content; unsafe; skip; } =
    pp fmt "{ @[<hov>content-type = %a;@ \
                     content-transfer-encoding = %a;@ \
                     version = %a;@ \
                     content-id = %a;@ \
                     content-description = %a;@ \
                     and @[<v>%a@]@] }"
      ContentType.pp ty
      ContentEncoding.pp encoding
      MimeVersion.pp version
      (pp_option MsgID.pp) id
      (pp_option pp_unstructured) description
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") (fun fmt (k, v) -> pp fmt "@[<hov>%s = %a@]" k pp_unstructured v))
        (Map.fold (fun k v acc -> (List.map (fun e -> (k, e)) v) @ acc) content [])


let default =
  { ty          = ContentType.default
  ; encoding    = ContentEncoding.default
  ; version     = MimeVersion.default
  ; id          = None
  ; description = None
  ; content     = Map.empty
  ; unsafe      = Map.empty
  ; skip        = [] }

module Encoder =
struct
  open Encoder

  let w_crlf k e = string "\r\n" k e

  let w_unstructured (l : Rfc5322.unstructured) =
    let open Wrap in
    let w_elem = function
      | `Text s -> string s
      | `CR n -> string (String.make n '\r')
      | `LF n -> string (String.make n '\n')
      | `CRLF -> string "\r\n"
      | `WSP  -> space
      | `Encoded (charset, raw) ->
        string "=?"
        $ string charset
        $ string "?"
        $ Address.Encoder.w_raw raw
        $ string "?="
    in List.fold_right w_elem l

  let w_field = function
    | #ContentType.field as x -> ContentType.Encoder.w_field x
    | #ContentEncoding.field as x -> ContentEncoding.Encoder.w_field x
    | `ContentID v ->
      string "Content-ID: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ MsgID.Encoder.w_msg_id v $ close_box) (unlift k))))
      $ w_crlf
    | `ContentDescription v ->
      string "Content-Description: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured v $ close_box) (unlift k))))
      $ w_crlf
    | `Content (field, v) ->
      string ("Content-" ^ field)
      $ string ":"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured v $ close_box) (unlift k))))
      $ w_crlf

  let w_field_version = function
    | #MimeVersion.field as x -> MimeVersion.Encoder.w_field x

  let w_unsafe = function
    | `Unsafe (field, v) ->
      string field
      $ string ":"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured v $ close_box) (unlift k))))
      $ w_crlf

  let w_skip = function
    | `Skip line -> string line $ w_crlf

  let w_message { ty; encoding; version; id; description; content; _ } =
    w_field (`ContentType ty)
    $ w_field (`ContentEncoding encoding)
    $ w_field_version (`MimeVersion version)
    $ (match id          with Some v -> w_field (`ContentID v) | None -> noop)
    $ (match description with Some v -> w_field (`ContentDescription v) | None -> noop)
    $ (Map.fold (fun field values acc -> List.fold_right (fun value -> w_field (`Content (field, value))) values $ acc) content noop)

  let w_part { ty; encoding; id; description; content; unsafe; skip; _ } =
    w_field (`ContentType ty)
    $ w_field (`ContentEncoding encoding)
    $ (match id          with Some v -> w_field (`ContentID v) | None -> noop)
    $ (match description with Some v -> w_field (`ContentDescription v) | None -> noop)
    $ (Map.fold (fun field values acc -> List.fold_right (fun value -> w_field (`Content (field, value))) values $ acc) content noop)
    $ (Map.fold (fun field values acc -> List.fold_right (fun value -> w_unsafe (`Unsafe (field, value))) values $ acc) unsafe noop)
end

open Parser

let message (fields : [> Rfc2045.field | Rfc2045.field_version ] list) =
  { f = fun i s fail succ ->
    let rec catch garbage acc = function
      | `ContentType content :: r ->
        catch garbage { acc with ty = content } r
      | `ContentEncoding mechanism :: r ->
        catch garbage { acc with encoding = mechanism } r
      | `ContentID id :: r ->
        catch garbage { acc with id = Some id } r
      | `ContentDescription desc :: r ->
        catch garbage { acc with description = Some desc } r
      | `MimeVersion version :: r ->
        catch garbage { acc with version = version } r
      | `Content (field_name, value) :: r ->
        let content =
          try let old = Map.find field_name acc.content in
              Map.add field_name (value :: old) acc.content
          with Not_found -> Map.add field_name [value] acc.content
        in
        catch garbage { acc with content = content } r
      | field :: r ->
        catch (field :: garbage) acc r
      | [] -> acc, List.rev garbage (* keep the order *)
    in

    succ i s (catch [] default fields) }

let part (fields : [> Rfc2045.field | Rfc2045.unsafe | Rfc2045.skip ] list) =
  { f = fun i s fail succ ->
    let rec catch garbage acc = function
      | `ContentType content :: r ->
        catch garbage { acc with ty = content } r
      | `ContentEncoding mechanism :: r ->
        catch garbage { acc with encoding = mechanism } r
      | `ContentID id :: r ->
        catch garbage { acc with id = Some id } r
      | `ContentDescription desc :: r ->
        catch garbage { acc with description = Some desc } r
      | `Content (field_name, value) :: r ->
        let content =
          try let old = Map.find field_name acc.content in
              Map.add field_name (value :: old) acc.content
          with Not_found -> Map.add field_name [value] acc.content
        in
        catch garbage { acc with content = content } r
      | `Unsafe (field_name, value) :: r ->
        let unsafe =
          try let old = Map.find field_name acc.unsafe in
              Map.add field_name (value :: old) acc.unsafe
          with Not_found -> Map.add field_name [value] acc.unsafe
        in
        catch garbage { acc with unsafe = unsafe } r
      | `Skip line :: r ->
        catch garbage { acc with skip = line :: acc.skip } r
      | field :: r ->
        catch (field :: garbage) acc r
      | [] -> acc, List.rev garbage
    in

    succ i s (catch [] default fields) }
