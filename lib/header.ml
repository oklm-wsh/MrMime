open Base

type phrase = [ `Phrase of Rfc5322.phrase ]

type t =
  { date          : Date.t
  ; from          : Address.person list
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
  ; comments      : Rfc5322.phrase list
  ; keywords      : Rfc5322.phrase list
  ; others        : (string * Rfc5322.phrase) list }

let of_lexer fields p state =
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
  let comments      = ref [] in
  let keywords      = ref [] in
  let others        = ref [] in

  let sanitize fields =
    match !date, !from with
    | Some date, Some from ->
      p (Some { date; from
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
        fields state
    | _ -> p None fields state
  in

  (* See RFC 5322 § 3.6:

     +----------------+--------+------------+----------------------------+
     | Field          | Min    | Max number | Notes                      |
     |                | number |            |                            |
     +-------------------------------------------------------------------+
     | orig-date      | 1      | 1          |                            |
     | from           | 1      | 1          | See sender and 3.6.2       |
     | sender         | 0*     | 1          | MUST occur with            |
     |                |        |            | multi-address from - see   |
     |                |        |            | 3.6.2                      |
     | reply-to       | 0      | 1          |                            |
     | to             | 0      | 1          |                            |
     | cc             | 0      | 1          |                            |
     | bcc            | 0      | 1          |                            |
     | message-id     | 0*     | 1          | SHOULD be present - see    |
     |                |        |            | 3.6.4                      |
     | in-reply-to    | 0*     | 1          | SHOULD occur in some       |
     |                |        |            | replies - see 3.6.4        |
     | references     | 0*     | 1          | SHOULD occur in some       |
     |                |        |            | replies - see 3.6.4        |
     | subject        | 0      | 1          |                            |
     | comments       | 0      | unlimited  |                            |
     | keywords       | 0      | unlimited  |                            |
     | optional-field | 0      | unlimited  |                            |
     +----------------+--------+------------+----------------------------+
  *)
  let rec loop garbage fields = match fields with
    | [] -> sanitize (List.rev garbage)
    | field :: rest ->
      match field with
      | `Date d ->
        (match !date with
         | None   -> date := Some (Date.of_lexer d);
                     loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Date" state)))
      | `From f ->
        (match !from with
         | None   -> from := Some (List.map Address.person_of_lexer f);
                     loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "From" state)))
      | `Sender c ->
        (match !sender with
         | None   -> sender := Some (Address.person_of_lexer c); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Sender" state)))
      | `ReplyTo r ->
        (match !reply_to with
         | None   -> reply_to := Some (Address.List.of_lexer r); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Reply-To" state)))
      | `To l ->
        (match !target with
         | None   -> target := Some (Address.List.of_lexer l); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "To" state)))
      | `Cc l ->
        (match !cc with
         | None   -> cc := Some (Address.List.of_lexer l); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Cc" state)))
      | `Bcc l ->
        (match !bcc with
         | None   -> bcc := Some (Address.List.of_lexer l); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Bcc" state)))
      | `MessageID m ->
        (match !msg_id with
         | None   -> msg_id := Some (MsgID.of_lexer m); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Message-ID" state)))
      | `InReplyTo l ->
        let f = List.map (function `MsgID m -> `MsgID (MsgID.of_lexer m)
                                 | #phrase as x -> x)
        in
        (match !in_reply_to with
         | None   -> in_reply_to := Some (f l); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "In-Reply-To" state)))
      | `References l ->
        let f = List.map (function `MsgID m -> `MsgID (MsgID.of_lexer m)
                                 | #phrase as x -> x)
        in
        (match !references with
         | None   -> references := Some (f l); loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "References" state)))
      | `Subject s ->
        (match !subject with
         | None   -> subject := Some s; loop garbage rest
         | Some _ -> raise (Lexer.Error (Lexer.err_unexpected_field "Subject" state)))
      | `Comments c ->
        comments := c :: !comments;
        loop garbage rest
      | `Keywords l ->
        keywords := l @ !keywords; loop garbage rest
      | `Field (field_name, value) ->
        others := (field_name, value) :: !others;
        loop garbage rest

      (* See RFC 5322 § 3.6:

         +----------------+--------+------------+----------------------------+
         | Field          | Min    | Max number | Notes                      |
         |                | number |            |                            |
         +-------------------------------------------------------------------+
         | resent-date    | 0*     | unlimited* | One per block, required if |
         |                |        |            | other resent fields are    |
         |                |        |            | present - see 3.6.6        |
         | resent-from    | 0      | unlimited* | One per block - see 3.6.6  |
         | resent-sender  | 0*     | unlimited* | One per block, MUST occur  |
         |                |        |            | with multi-address         |
         |                |        |            | resent-from - see 3.6.6    |
         | resent-to      | 0      | unlimited* | One per block - see 3.6.6  |
         | resent-cc      | 0      | unlimited* | One per block - see 3.6.6  |
         | resent-bcc     | 0      | unlimited* | One per block - see 3.6.6  |
         | resent-msg-id  | 0      | unlimited* | One per block - see 3.6.6  |
         +-------------------------------------------------------------------+
      *)
      | `ResentDate _ | `ResentFrom _ | `ResentSender _ | `ResentTo _
      | `ResentCc _   | `ResentBcc _  | `ResentMessageID _ | `ResentReplyTo _ ->
        Resent.of_lexer
          (field :: rest)
          (function
           | Some resent -> fun fields state -> resents := resent :: !resents;
                                                loop garbage fields
           | None        -> fun fields state -> loop garbage fields)
          state

      (* See RFC 5322 § 3.6:

         +----------------+--------+------------+----------------------------+
         | Field          | Min    | Max number | Notes                      |
         |                | number |            |                            |
         +----------------+--------+------------+----------------------------+
         | trace          | 0      | unlimited  | Block prepended - see      |
         |                |        |            | 3.6.7                      |
         +-------------------------------------------------------------------+
      *)
      | `ReturnPath _ | `Received _ ->
        Trace.of_lexer
          (field :: rest)
          (function
           | Some trace -> fun fields state -> traces := trace :: !traces;
                                               loop garbage fields
           | None       -> fun fields state -> loop garbage fields)
          state
  in

  loop [] fields

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
    | `Ok (data, state) -> of_lexer data (fun x rest state -> x) state
  in

  let rule = Rfc5322.p_header
    (fun field p state -> raise (Lexer.Error (Lexer.err_nothing_to_do state)))
    (fun fields state -> `Ok (fields, state)) in

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
  { date; from
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
  @:@ (comments >|= fun l -> `Comments l)
  @:@ (keywords >|= fun l -> `Keywords l)
  @:@ (others >|= fun l -> `Others l)
  @:@ (`From from) :: (`Date date) :: []

let pp_ext fmt = function
  | `Phrase l -> p fmt "%a" pp_phrase l
  | `MsgID m  -> p fmt "%a" MsgID.pp m

let pp_field fmt (field_name, field) =
  p fmt "%s: %a\r\n" field_name pp_phrase field

let pp_field fmt = function
  | `From l            -> p fmt "From: %a\r\n"
                            (pp_list ~sep:", " Address.pp_person) l
  | `Date d            -> p fmt "Date: %a\r\n" Date.pp d
  | `Content c         -> p fmt "%a" Content.pp c
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
  | `Comments l        -> p fmt "%a" (pp_list (fun fmt x -> p fmt "Comments: %a\r\n" pp_phrase x)) l
  | `Keywords l        -> p fmt "Keywords: %a\r\n"
                            (pp_list ~sep:"," pp_phrase) l
  | `Others l          -> p fmt "%a" (pp_list pp_field) l

let pp fmt t =
  p fmt "%a" (pp_list pp_field) (t_to_list t)

let to_string t =
  let tmp = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer tmp in

  Format.fprintf fmt "%a%!" pp t;
  Buffer.contents tmp

let equal = (=)
