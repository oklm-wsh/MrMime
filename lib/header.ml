open Base

type phrase = [ `Phrase of Rfc5322.phrase ]

type t =
  { date          : Date.t
  ; from          : Address.person list
  ; content       : ContentType.t
  ; version       : Version.t
  ; encoding      : Encoding.t
  ; sender        : Address.person option
  ; reply_to      : Address.List.t option
  ; target        : Address.List.t option
  ; cc            : Address.List.t option
  ; bcc           : Address.List.t option
  ; subject       : Rfc5322.phrase option
  ; msg_id        : MsgID.t option
  ; in_reply_to   : [ phrase | `MsgID of MsgID.t ] list
  ; references    : [ phrase | `MsgID of MsgID.t ] list
  ; resents       : Resent.t list
  ; traces        : Trace.t list
  ; comments      : Rfc5322.phrase option
  ; keywords      : Rfc5322.phrase list
  ; others        : (string * Rfc5322.phrase) list }

let of_lexer k l =
  let from          = ref None in
  let date          = ref None in
  let sender        = ref None in
  let reply_to      = ref None in
  let target        = ref None in
  let cc            = ref None in
  let bcc           = ref None in
  let subject       = ref None in
  let msg_id        = ref None in
  let in_reply_to   = ref None in
  let references    = ref None in
  let resents       = ref [] in
  let traces        = ref [] in
  let comments      = ref None in
  let keywords      = ref [] in
  let content       = ref None in
  let version       = ref None in
  let encoding      = ref None in
  let others        = ref [] in

  let sanitize fields =
    match !date, !from with
    | Some date, Some from ->
      k (Some { date; from
              ; content     = Option.value ~default:ContentType.default !content
              ; version     = Option.value ~default:Version.default !version
              ; encoding    = Option.value ~default:Encoding.default !encoding
              ; sender      = !sender
              ; reply_to    = !reply_to
              ; target      = !target
              ; cc          = !cc
              ; bcc         = !bcc
              ; subject     = !subject
              ; msg_id      = !msg_id
              ; in_reply_to = Option.value ~default:[] !in_reply_to
              ; references  = Option.value ~default:[] !references
              ; resents     = !resents
              ; traces      = !traces
              ; comments    = !comments
              ; keywords    = !keywords
              ; others      = !others })
        fields
    | _ -> k None fields
  in

  let rec loop i l = match l with
    | [] -> sanitize (List.rev i)
    | x :: rest ->
      match x with
      | `Date d ->
        (match !date with
         | None   -> date := Some (Date.of_lexer d); loop i rest
         (* XXX: may be it's an error *)
         | Some _ -> loop i rest)
      | `From f ->
        (match !from with
         | None   -> from := Some (List.map Address.person_of_lexer f);
                     loop i rest
         (* XXX: may be it's an error *)
         | Some _ -> loop i rest)
      | `ContentType c ->
        (match !content with
         | None   -> content := Some (ContentType.of_lexer c);
                     loop i rest
         | Some _ -> loop i rest)
      | `MIMEVersion v ->
        (match !content with
         | None   -> version := Some (Version.of_lexer v);
                     loop i rest
         | Some _ -> loop i rest)
      | `ContentEncoding v ->
        (match !content with
         | None   -> encoding := Some (Encoding.of_lexer v);
                     loop i rest
         | Some _ -> loop i rest)
      | `Sender c ->
        (match !sender with
         | None   -> sender := Some (Address.person_of_lexer c); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ReplyTo r ->
        (match !reply_to with
         | None   -> reply_to := Some (Address.List.of_lexer r); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `To l ->
        (match !target with
         | None   -> target := Some (Address.List.of_lexer l); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `Cc l ->
        (match !cc with
         | None   -> cc := Some (Address.List.of_lexer l); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `Bcc l ->
        (match !bcc with
         | None   -> bcc := Some (Address.List.of_lexer l); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `Subject s ->
        (match !subject with
         | None   -> subject := Some s; loop i rest
         | Some _ -> loop (x :: i) rest)
      | `MessageID m ->
        (match !msg_id with
         | None   -> msg_id := Some (MsgID.of_lexer m); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `InReplyTo l ->
        let f = List.map (function `MsgID m -> `MsgID (MsgID.of_lexer m)
                                 | #phrase as x -> x)
        in
        (match !in_reply_to with
         | None   -> in_reply_to := Some (f l); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `References l ->
        let f = List.map (function `MsgID m -> `MsgID (MsgID.of_lexer m)
                                 | #phrase as x -> x)
        in
        (match !in_reply_to with
         | None   -> references := Some (f l); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ResentDate _ | `ResentFrom _ | `ResentSender _ | `ResentTo _
      | `ResentCc _   | `ResentBcc _  | `ResentMessageID _ ->
        Resent.of_lexer
          (function
           | Some resent -> fun l -> resents := resent :: !resents; loop i l
           | None        -> fun l -> loop i l)
          (x :: rest)
      | `ReturnPath _ | `Received _ ->
        Trace.of_lexer
          (function
           | Some trace -> fun l -> traces := trace :: !traces; loop i l
           | None       -> fun l -> loop i l)
          (x :: rest)
      | `Comments s ->
        (match !comments with
         | None   -> comments := Some s; loop i rest
         | Some _ -> loop (x :: i) rest)
      | `Keywords l ->
        keywords := l @ !keywords; loop i rest
      | `Field (field_name, value) ->
        others := (field_name, value) :: !others;
        loop i rest
  in

  loop [] l

let of_string s =
  let rec loop = function
    | `Error (exn, buf, off, len) ->
      let tmp = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer tmp in

      Format.fprintf fmt "%a (buf: %S)%!"
        Lexer.pp_error exn (Bytes.sub buf off (len - off));

      raise (Invalid_argument ("Header.of_string: " ^ (Buffer.contents tmp)))
    | `Read (buf, off, len, k) ->
      raise (Invalid_argument "Header.of_string: unterminated string")
    | `Ok data -> of_lexer (fun x rest -> x) data
  in

  let rule = Rfc5322.p_header (fun data state -> `Ok data) in

  match loop @@ Lexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n")) with
  | Some header -> header
  | None -> raise (Invalid_argument "Header.of_string: incomplete header")

let p = Format.fprintf

let pp_list ?(last = false) ?(sep = "") pp_data fmt lst =
  let rec aux = function
    | [] -> if last then p fmt "%s" sep else ()
    | [ x ] -> pp_data fmt x
    | x :: r -> p fmt "%a%s" pp_data x sep; aux r
  in

  aux lst

let t_to_list
  { date; from; content; version; encoding
  ; sender; reply_to; target; cc; bcc; subject; msg_id
  ; in_reply_to; references
  ; resents; traces
  ; comments; keywords
  ; others } =
  let ( >>= ) o f = match o with Some x -> Some (f x) | None -> None in
  let ( @:@ ) o r = match o with Some x -> x :: r | None -> r in
  let ( >|= ) l f = match l with [] -> None | l -> Some (f l) in

  (sender >>= fun p -> `Sender p)
  @:@ (reply_to >>= fun l -> `ReplyTo l)
  @:@ (target >>= fun l -> `To l)
  @:@ (cc >>= fun l -> `Cc l)
  @:@ (bcc >>= fun l -> `Bcc l)
  @:@ (subject >>= fun s -> `Subject s)
  @:@ (msg_id >>= fun m -> `MessageID m)
  @:@ (in_reply_to >|= fun l -> `InReplyTo l)
  @:@ (references >|= fun l -> `References l)
  @:@ (resents >|= fun l -> `Resent l)
  @:@ (traces >|= fun l -> `Trace l)
  @:@ (comments >>= fun s -> `Comments s)
  @:@ (keywords >|= fun l -> `Keywords l)
  @:@ (others >|= fun l -> `Others l)
  @:@ (`ContentEncoding encoding) :: (`MIMEVersion version) :: (`ContentType content) :: (`From from) :: (`Date date) :: []

let pp_ext fmt = function
  | `Phrase l -> p fmt "%a" pp_phrase l
  | `MsgID m  -> p fmt "%a" MsgID.pp m

let pp_field fmt (field_name, field) =
  p fmt "%s: %a\r\n" field_name pp_phrase field

let pp_field fmt = function
  | `From l            -> p fmt "From: %a\r\n"
                            (pp_list ~sep:", " Address.pp_person) l
  | `Date d            -> p fmt "Date: %a\r\n" Date.pp d
  | `Sender e          -> p fmt "Sender: %a\r\n" Address.pp_person e
  | `ReplyTo l         -> p fmt "Reply-To: %a\r\n" Address.List.pp l
  | `To l              -> p fmt "To: %a\r\n" Address.List.pp l
  | `Cc l              -> p fmt "Cc: %a\r\n" Address.List.pp l
  | `Bcc l             -> p fmt "Bcc: %a\r\n" Address.List.pp l
  | `Subject s         -> p fmt "Subject: %a\r\n" pp_phrase s
  | `MessageID m       -> p fmt "Message-ID: %a\r\n" MsgID.pp m
  | `InReplyTo l       -> p fmt "In-Reply-To: %a\r\n"
                            (pp_list ~sep:" " pp_ext) l
  | `References l      -> p fmt "References: %a\r\n"
                            (pp_list ~sep:" " pp_ext) l
  | `Resent l          -> p fmt "%a" (pp_list Resent.pp) l
  | `Trace l           -> p fmt "%a" (pp_list Trace.pp) l
  | `Comments s        -> p fmt "Comments: %a\r\n" pp_phrase s
  | `Keywords l        -> p fmt "Keywords: %a\r\n"
                            (pp_list ~sep:"," pp_phrase) l
  | `MIMEVersion v     -> p fmt "MIME-Version: %a\r\n" Version.pp v
  | `ContentType c     -> p fmt "Content-Type: %a\r\n" ContentType.pp c
  | `ContentEncoding e -> p fmt "Content-Encoding: %a\r\n" Encoding.pp e
  | `Others l          -> p fmt "%a" (pp_list pp_field) l

let pp fmt t =
  p fmt "%a" (pp_list pp_field) (t_to_list t)

let to_string t =
  let tmp = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer tmp in

  Format.fprintf fmt "%a%!" pp t;
  Buffer.contents tmp

let equal = (=)
