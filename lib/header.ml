type raw              = Rfc2047.raw = QuotedPrintable of string | Base64 of Base64.result
type unstructured     = Rfc5322.unstructured
type phrase_or_msg_id = Rfc5322.phrase_or_msg_id
type field            = Rfc5322.field

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

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

let pp_phrase_or_msg_id fmt = function
  | `Phrase p -> pp fmt "%a" Address.pp_phrase p
  | `MsgID m  -> pp fmt "%a" MsgID.pp m

let pp_path = Address.pp_mailbox'

let pp_received fmt r =
  let pp_elem fmt = function
    | `Addr v -> Address.pp_mailbox' fmt v
    | `Domain v -> Address.pp_domain fmt v
    | `Word v -> Address.pp_word fmt v
  in
  match r with
  | (l, Some date) ->
    pp fmt "Received = { @[<hov>%a;@ date = %a@] }"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@ ") pp_elem) l
      Date.pp date
  | (l, None) ->
    pp fmt "Received = @[<hov>%a@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@ ") pp_elem) l

let pp_option pp_data fmt = function
  | Some v -> pp_data fmt v
  | None -> Format.pp_print_string fmt "<none>"

let pp_field fmt = function
  | `Date v            -> pp fmt "@[<hov>Date = %a@]" Date.pp v
  | `From v            -> pp fmt "@[<hov>From = @[<v>%a@]@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") Address.pp_mailbox) v
  | `Sender v          -> pp fmt "@[<hov>Sender = %a@]" Address.pp_mailbox v
  | `ReplyTo v         -> pp fmt "@[<hov>Reply-To = %a@]" Address.List.pp v
  | `To v              -> pp fmt "@[<hov>To = %a@]" Address.List.pp v
  | `Cc v              -> pp fmt "@[<hov>Cc = %a@]" Address.List.pp v
  | `Bcc v             -> pp fmt "@[<hov>Bcc = %a@]" Address.List.pp v
  | `MessageID v       -> pp fmt "@[<hov>Message-ID = %a@]" MsgID.pp v
  | `InReplyTo v       -> pp fmt "@[<hov>In-Reply-To = @[<v>%a@]@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_phrase_or_msg_id) v
  | `References v      -> pp fmt "@[<hov>References = @[<v>%a@]@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_phrase_or_msg_id) v
  | `Subject v         -> pp fmt "@[<hov>Subject = %a@]" pp_unstructured v
  | `Comments v        -> pp fmt "@[<hov>Comments = %a@]" pp_unstructured v
  | `Keywords v        -> pp fmt "@[<hov>Keywords = @[<v>%a@]@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") Address.pp_phrase) v
  | `ResentDate v      -> pp fmt "@[<hov>Resent-Date = %a@]" Date.pp v
  | `ResentFrom v      -> pp fmt "@[<hov>Resent-From = @[<v>%a@]@]"
      (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") Address.pp_mailbox) v
  | `ResentSender v    -> pp fmt "@[<hov>Resent-Sender = %a@]" Address.pp_mailbox v
  | `ResentReplyTo v   -> pp fmt "@[<hov>Resent-Reply-To = %a@]" Address.List.pp v
  | `ResentTo v        -> pp fmt "@[<hov>Resent-To = %a@]" Address.List.pp v
  | `ResentCc v        -> pp fmt "@[<hov>Resent-Cc = %a@]" Address.List.pp v
  | `ResentBcc v       -> pp fmt "@[<hov>Resent-Bcc = %a@]" Address.List.pp v
  | `ResentMessageID v -> pp fmt "@[<hov>Resent-Message-ID = %a@]" MsgID.pp v
  | `Field (k, v)      -> pp fmt "@[<hov>%s = %a@]" (String.capitalize_ascii k) pp_unstructured v
  | `Unsafe (k, v)     -> pp fmt "@[<hov>%s # %a@]" (String.capitalize_ascii k) pp_unstructured v
  | `Trace (Some p, r) ->
    pp fmt "@[<hov>Return-Path = %a@]@\n& %a"
      Trace.pp_path p
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n& ") Trace.pp_received) r
  | `Trace (None, r)   ->
    pp fmt "%a"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n& ") Trace.pp_received) r
  | `Skip line         -> pp fmt "@[<hov># %S@]" line

module Map = Map.Make(String)

type header =
  { date        : Date.date option
  ; from        : Address.mailbox list
  ; sender      : Address.mailbox option
  ; reply_to    : Address.address list
  ; to'         : Address.address list
  ; cc          : Address.address list
  ; bcc         : Address.address list
  ; subject     : unstructured option
  ; msg_id      : MsgID.msg_id option
  ; in_reply_to : phrase_or_msg_id list
  ; references  : phrase_or_msg_id list
  ; comments    : unstructured list
  ; keywords    : Address.phrase list list
  ; resents     : Resent.resent list
  ; traces      : Trace.trace list
  ; fields      : unstructured list Map.t
  ; unsafe      : unstructured list Map.t
  ; skip        : string list }

let pp_map fmt map =
  Map.iter
    (fun key value -> pp fmt "%s -> [@[<hov>%a@]]@\n" key (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") pp_unstructured) value)
    map

let pp fmt { date; from; sender; reply_to; to'; cc; bcc; subject;
             msg_id; in_reply_to; references; comments; keywords;
             resents; traces; fields; unsafe; skip; } =
  pp fmt "{ @[<hov>date = @[<hov>%a@];@ \
                   from = @[<v>%a@];@ \
                   sender = @[<hov>%a@];@ \
                   reply-to = @[<hov>%a@];@ \
                   to = @[<hov>%a@];@ \
                   cc = @[<hov>%a@];@ \
                   bcc = @[<hov>%a@];@ \
                   subject = @[<hov>%a@];@ \
                   msg-id = @[<hov>%a@];@ \
                   in-reply-to = @[<hov>%a@];@ \
                   references = @[<hov>%a@];@ \
                   comments = @[<hov>%a@];@ \
                   keywords = @[<hov>%a@];@ \
                   resents = @[<hov>%a@];@ \
                   traces = @[<hov>%a@];@ \
                   fields = @[<hov>%a@];@ \
                   unsafe = @[<hov>%a@];@ \
                   skip = %a;@] }"
    (pp_option Date.pp) date
    (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") Address.pp_mailbox) from
    (pp_option Address.pp_mailbox) sender
    Address.List.pp reply_to
    Address.List.pp to'
    Address.List.pp cc
    Address.List.pp bcc
    (pp_option pp_unstructured) subject
    (pp_option MsgID.pp) msg_id
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_phrase_or_msg_id) in_reply_to
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_phrase_or_msg_id) references
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") pp_unstructured) comments
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n")
     (pp_lst ~sep:(fun fmt () -> pp fmt ",@ ") Address.pp_phrase)) keywords
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") Resent.pp) resents
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") Trace.pp) traces
    pp_map fields
    pp_map unsafe
    (pp_lst ~sep:(fun fmt () -> pp fmt "@\n") Format.pp_print_string) skip

let default =
  { date        = None
  ; from        = []
  ; sender      = None
  ; reply_to    = []
  ; to'         = []
  ; cc          = []
  ; bcc         = []
  ; subject     = None
  ; msg_id      = None
  ; in_reply_to = []
  ; references  = []
  ; comments    = []
  ; keywords    = []
  ; resents     = []
  ; traces      = []
  ; fields      = Map.empty
  ; unsafe      = Map.empty
  ; skip        = [] }

module Encoder =
struct
  include Encoder

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

  let w_crlf k e = string "\r\n" k e

  let rec w_lst w_sep w_data l =
    let open Wrap in
      let rec aux = function
      | [] -> noop
      | [ x ] -> w_data x
      | x :: r -> w_data x $ w_sep $ aux r
    in aux l

  let w_field (field : [ Rfc5322.field_header | Rfc5322.skip ]) = match field with
    | `Bcc l ->
      string "Bcc: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `Cc l ->
      string "Cc: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `Subject p ->
      string "Subject:"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured p $ close_box) (unlift k))))
      $ w_crlf
    | `To l ->
      string "To: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `References l ->
      let w_data = function
        | `Phrase p -> Address.Encoder.w_phrase p
        | `MsgID m -> MsgID.Encoder.w_msg_id m
      in
      string "References: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst space w_data l $ close_box) (unlift k))))
      $ w_crlf
    | `Field (key, value) ->
      string key $ string ":"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured value $ close_box) (unlift k))))
      $ w_crlf
    | `Date d ->
      string "Date: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Date.Encoder.w_date d $ close_box) (unlift k))))
      $ w_crlf
    | `InReplyTo l ->
      let w_data = function
        | `Phrase p -> Address.Encoder.w_phrase p
        | `MsgID m -> MsgID.Encoder.w_msg_id m
      in
      string "In-Reply-To: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst space w_data l $ close_box) (unlift k))))
      $ w_crlf
    | `MessageID m ->
      string "Message-ID: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ MsgID.Encoder.w_msg_id m $ close_box) (unlift k))))
      $ w_crlf
    | `Comments p ->
      string "Comments:"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured p $ close_box) (unlift k))))
      $ w_crlf
    | `From l ->
      string "From: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst (string "," $ space) Address.Encoder.w_mailbox l $ close_box) (unlift k))))
      $ w_crlf
    | `Sender p ->
      string "Sender: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_mailbox p $ close_box) (unlift k))))
      $ w_crlf
    | `Unsafe (key, value) ->
      string key $ string ":"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_unstructured value $ close_box) (unlift k))))
      $ w_crlf
    | `Keywords l ->
      string "Keywords:"
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst (string "," $ space) Address.Encoder.w_phrase l $ close_box) (unlift k))))
      $ w_crlf
    | `ReplyTo l ->
      string "Reply-To: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `Skip l ->
      string l $ w_crlf

  let w_header { date; from; sender; reply_to; to'; cc; bcc; subject;
                 msg_id; in_reply_to; references; comments; keywords;
                 resents; traces; fields; unsafe; skip; } =
    (match date        with Some v -> w_field (`Date v) | None -> noop)
    $ (match from        with [] -> noop | v -> w_field (`From v))
    $ (match sender      with Some v -> w_field (`Sender v) | None -> noop)
    $ (match reply_to    with [] -> noop | v -> w_field (`ReplyTo v))
    $ (match to'         with [] -> noop | v -> w_field (`To v))
    $ (match cc          with [] -> noop | v -> w_field (`Cc v))
    $ (match bcc         with [] -> noop | v -> w_field (`Bcc v))
    $ (match subject     with Some v -> w_field (`Subject v) | None -> noop)
    $ (match msg_id      with Some v -> w_field (`MessageID v) | None -> noop)
    $ (match in_reply_to with [] -> noop | v -> w_field (`InReplyTo v))
    $ (match references  with [] -> noop | v -> w_field (`References v))
    $ List.fold_right (fun v -> w_field (`Comments v)) comments
    $ List.fold_right (fun v -> w_field (`Keywords v)) keywords
    $ List.fold_right Resent.Encoder.w_resent resents
    $ List.fold_right Trace.Encoder.w_trace traces
    $ (Map.fold (fun field values acc -> List.fold_right (fun value -> w_field (`Field (field, value))) values $ acc) fields noop)
    $ (Map.fold (fun field values acc -> List.fold_right (fun value -> w_field (`Unsafe (field, value))) values $ acc) unsafe noop)
end

let to_string t =
  let buf   = Buffer.create 16 in
  let state = Encoder.make () in

  let rec loop = function
    | `Partial (s, i, l, k) ->
      Buffer.add_subbytes buf s i l;
      loop @@ (k l)
    | `Ok -> Buffer.contents buf
  in

  loop @@ Encoder.w_header t (Encoder.flush (fun _ -> `Ok)) state

open Parser

let decoder (fields : [> field ] list) =
  { f = fun i s fail succ ->
    let rec catch garbage acc = function
      | `Date date :: r ->
        catch garbage { acc with date = Some date } r
      | `From lst :: r ->
        catch garbage { acc with from = lst @ acc.from } r
      | `Sender mail :: r ->
        catch garbage { acc with sender = Some mail } r
      | `ReplyTo lst :: r ->
        catch garbage { acc with reply_to = lst @ acc.reply_to } r
      | `To lst :: r ->
        catch garbage { acc with to' = lst @ acc.to' } r
      | `Cc lst :: r ->
        catch garbage { acc with cc = lst @ acc.cc } r
      | `Bcc lst :: r ->
        catch garbage { acc with bcc = lst @ acc.bcc } r
      | `Subject subject :: r ->
        catch garbage { acc with subject = Some subject } r
      | `MessageID msg_id :: r ->
        catch garbage { acc with msg_id = Some msg_id } r
      | `InReplyTo lst :: r->
        catch garbage { acc with in_reply_to = lst @ acc.in_reply_to } r
      | `References lst :: r ->
        catch garbage { acc with references = lst @ acc.references } r
      | `Comments lst :: r ->
        catch garbage { acc with comments = lst :: acc.comments } r
      | `Keywords lst :: r ->
        catch garbage { acc with keywords = lst :: acc.keywords } r
      | `Field (field_name, value) :: r ->
        let fields =
          try let old = Map.find field_name acc.fields in
              Map.add field_name (value :: old) acc.fields
          with Not_found -> Map.add field_name [value] acc.fields
        in
        catch garbage { acc with fields = fields } r
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
      | [] -> acc, List.rev garbage (* keep the order *)
    in

    succ i s (catch [] default fields) }
  >>= fun (header, fields) -> Trace.decoder fields
  >>= fun (traces, fields) -> return ({ header with traces = traces }, fields)
  >>= fun (header, fields) -> Resent.decoder fields
  >>= fun (resents, fields) -> return ({ header with resents = resents }, fields)

let of_string ?(chunk = 1024) s =
  let s' = s ^ "\r\n" in
  let l = String.length s' in
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Fail _ -> None
    | Read { buffer; k; } ->
      let n = min chunk (l - consumed) in
      Input.write_string buffer s' consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Complete else Incomplete)
    | Done v -> Some v
  in

  aux 0 @@ run i (Rfc5322.header (fun _ -> fail Rfc5322.Nothing_to_do) >>= decoder <* Rfc822.crlf)

let of_string_raw ?(chunk = 1024) s off len =
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Fail _ -> None
    | Read { buffer; k; } ->
      let n = min chunk (len - (consumed - off)) in
      Input.write_string buffer s consumed n;
      aux (consumed + n) @@ k n (if (consumed + n - off) = len then Complete else Incomplete)
    | Done v -> Some (v, consumed - off)
  in

  aux off @@ run i (Rfc5322.header (fun _ -> fail Rfc5322.Nothing_to_do) >>= decoder)

let equal a b =
  a.date = b.date
  && a.from = b.from
  && a.sender = b.sender
  && a.reply_to = b.reply_to
  && a.to' = b.to'
  && a.cc = b.cc
  && a.bcc = b.bcc
  && a.subject = b.subject
  && a.msg_id = b.msg_id
  && a.in_reply_to = b.in_reply_to
  && a.references = b.references
  && a.comments = b.comments
  && a.keywords = b.keywords
  && (List.fold_left (&&) true (List.map2 Resent.equal a.resents b.resents))
  && (List.fold_left (&&) true (List.map2 Trace.equal a.traces b.traces))
  && Map.equal (=) a.fields b.fields
  && Map.equal (=) a.unsafe b.unsafe
  && a.skip = b.skip
