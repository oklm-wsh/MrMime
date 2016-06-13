type phrase = [ `Phrase of Rfc5322.phrase ]

type ('date, 'from) t =
  { date          : 'date
  ; from          : 'from
  ; sender        : Address.person option
  ; reply_to      : Address.t list option
  ; target        : Address.t list option
  ; cc            : Address.t list option
  ; bcc           : Address.t list option
  ; subject       : Rfc5322.phrase option
  ; msg_id        : MsgID.t option
  ; in_reply_to   : [ phrase | `MsgID of MsgID.t ] list
  ; references    : [ phrase | `MsgID of MsgID.t ] list
  ; resents       : Resent.t list
  ; traces        : Trace.t list
  ; comments      : Rfc5322.phrase list
  ; keywords      : Rfc5322.phrase list
  ; others        : (string * Rfc5322.phrase) list
  ; unsafe        : (string * Rfc5322.phrase) list }

type strict = (Date.t, Address.person list) t
type unstrict = (Date.t option, Address.person list option) t

type field =
  [ `From        of Address.person list
  | `Date        of Date.t
  | `Sender      of Address.person
  | `ReplyTo     of Address.t list
  | `To          of Address.t list
  | `Cc          of Address.t list
  | `Bcc         of Address.t list
  | `Subject     of Rfc5322.phrase
  | `Comments    of Rfc5322.phrase
  | `Keywords    of Rfc5322.phrase list
  | `MessageID   of MsgID.t
  | `InReplyTo   of [ phrase | `MsgID of MsgID.t ] list
  | `References  of [ phrase | `MsgID of MsgID.t ] list
  | Resent.field
  | Trace.field
  | `Field       of string * Rfc5322.phrase
  | `Unsafe      of string * Rfc5322.phrase ]

module Relax =
struct
  exception Expected_date
  exception Expected_from

  type ('date, 'from) t =
    { date : Date.t option -> 'date
    ; from : Address.person list option -> 'from }

  let strict =
    { date =
      (function Some date -> date
              | None -> raise Expected_date)
    ; from =
      (function Some l -> l
              | None -> raise Expected_from) }

  let unstrict =
    { date = (fun x -> x)
    ; from = (fun x -> x) }
end

let field_of_lexer : Rfc5322.field -> field = function
  | `From l              -> `From (List.map Address.D.person_of_lexer l)
  | `Date d              -> `Date (Date.D.of_lexer d)
  | `Sender p            -> `Sender (Address.D.person_of_lexer p)
  | `ReplyTo l           -> `ReplyTo (Address.List.D.of_lexer l)
  | `To l                -> `To (Address.List.D.of_lexer l)
  | `Cc l                -> `Cc (Address.List.D.of_lexer l)
  | `Bcc l               -> `Bcc (Address.List.D.of_lexer l)
  | `Subject p           -> `Subject p
  | `Comments c          -> `Comments c
  | `Keywords l          -> `Keywords l
  | `MessageID m         -> `MessageID (MsgID.D.of_lexer m)
  | `InReplyTo l         ->
    let l = List.map (function `MsgID m     -> `MsgID (MsgID.D.of_lexer m)
                             | #phrase as x -> x) l
    in `InReplyTo l
  | `References l        ->
    let l = List.map (function `MsgID m     -> `MsgID (MsgID.D.of_lexer m)
                             | #phrase as x -> x) l
    in `References l
  | #Rfc5322.resent as x -> (Resent.field_of_lexer x :> field)
  | #Rfc5322.trace as x  -> (Trace.field_of_lexer x :> field)
  | `Field v             -> `Field v
  | `Unsafe v            -> `Unsafe v

let to_field header =
  let ( >>= ) o f = match o with Some x -> Some (f x) | None -> None in
  let ( @:@ ) o r = match o with Some x -> x :: r | None -> r in
  let ( >|= ) l f = match l with [] -> None | l -> Some (f l) in

  (header.date            >>= fun d -> `Date d)
  @:@ (header.from        >>= fun f -> `From f)
  @:@ (header.sender      >>= fun p -> `Sender p)
  @:@ (header.reply_to    >>= fun l -> `ReplyTo l)
  @:@ (header.target      >>= fun l -> `To l)
  @:@ (header.cc          >>= fun l -> `Cc l)
  @:@ (header.bcc         >>= fun l -> `Bcc l)
  @:@ (header.subject     >>= fun p -> `Subject p)
  @:@ (header.msg_id      >>= fun m -> `MessageID m)
  @:@ (header.in_reply_to >|= fun l -> `InReplyTo l)
  @:@ (header.references  >|= fun l -> `References l)
  @:@ []
  @ (List.map (fun (key, value) -> `Unsafe (key, value)) header.unsafe)
  @ (List.map (fun (key, value) -> `Field (key, value)) header.others)
  @ (List.concat @@ ((List.map Resent.to_field header.resents) :> field list list))
  @ (List.concat @@ ((List.map Trace.to_field header.traces) :> field list list))

module D =
struct
  let of_lexer relax fields p state =
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
    let unsafe        = ref [] in

    let sanitize fields =
      let date = relax.Relax.date !date in
      let from = relax.Relax.from !from in
      p ({ date; from
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
         ; others      = List.rev !others
         ; unsafe      = List.rev !unsafe })
        fields state
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
           | None   -> date := Some (Date.D.of_lexer d);
                       loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Date" state)))
        | `From f ->
          (match !from with
           | None   -> from := Some (List.map Address.D.person_of_lexer f);
                       loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "From" state)))
        | `Sender c ->
          (match !sender with
           | None   -> sender := Some (Address.D.person_of_lexer c); loop garbage rest
           | Some _ -> loop garbage rest)
           (* XXX: some email can have multiple sender, it's illegal but fuck the
                   world! *)
        | `ReplyTo r ->
          (match !reply_to with
           | None   -> reply_to := Some (Address.List.D.of_lexer r); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Reply-To" state)))
        | `To l ->
          (match !target with
           | None   -> target := Some (Address.List.D.of_lexer l); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "To" state)))
        | `Cc l ->
          (match !cc with
           | None   -> cc := Some (Address.List.D.of_lexer l); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Cc" state)))
        | `Bcc l ->
          (match !bcc with
           | None   -> bcc := Some (Address.List.D.of_lexer l); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Bcc" state)))
        | `MessageID m ->
          (match !msg_id with
           | None   -> msg_id := Some (MsgID.D.of_lexer m); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Message-ID" state)))
        | `InReplyTo l ->
          let func = List.map (function `MsgID m -> `MsgID (MsgID.D.of_lexer m)
                                      | #phrase as x -> x)
          in
          (match !in_reply_to with
           | None   -> in_reply_to := Some (func l); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "In-Reply-To" state)))
        | `References l ->
          let f = List.map (function `MsgID m -> `MsgID (MsgID.D.of_lexer m)
                                   | #phrase as x -> x)
          in
          (match !references with
           | None   -> references := Some (f l); loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "References" state)))
        | `Subject s ->
          (match !subject with
           | None   -> subject := Some s; loop garbage rest
           | Some _ -> raise (Error.Error (Error.err_unexpected_field "Subject" state)))
        | `Comments c ->
          comments := c :: !comments;
          loop garbage rest
        | `Keywords l ->
          keywords := l @ !keywords; loop garbage rest

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
        | #Rfc5322.resent ->
          Resent.D.of_lexer
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
        | #Rfc5322.trace ->
          Trace.D.of_lexer
            (field :: rest)
            (function
             | Some trace -> fun fields state -> traces := trace :: !traces;
                                                 loop garbage fields
             | None       -> fun fields state -> loop garbage fields)
            state

        | `Field (field_name, value) ->
          others := (field_name, value) :: !others;
          loop garbage rest
        | `Unsafe (field, value) ->
          unsafe := (field, value) :: !unsafe;
          loop garbage rest
        | field -> loop (field :: garbage) rest
    in

    loop [] fields

  open BaseDecoder

  let of_decoder state =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Error.pp exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("Header.of_string: " ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Header.of_string: unterminated string")
      | `Ok (data, state) -> of_lexer Relax.unstrict data (fun x rest state -> x) state
    in

    let rule = Rfc5322.p_header
      (fun field p state -> raise (Error.Error (Error.err_nothing_to_do state)))
      @ fun fields -> Rfc822.p_crlf
      @ Rfc822.p_crlf
      @ fun state -> `Ok (fields, state)
    in

    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_crlf k e = w "\r\n" k e

    let w_unstrict_header fields =
      let rec w_lst w_sep w_data l =
        let open Wrap in
          let rec aux = function
          | [] -> noop
          | [ x ] -> w_data x
          | x :: r -> w_data x $ w_sep $ aux r
        in aux l
      in
      List.fold_right
        (function
         | `ResentCc l ->
           w "Resent-Cc: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Cc: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentMessageID m ->
           w "Resent-Message-ID: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Message-ID: ")
                             $ MsgID.E.w m
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Bcc l ->
           w "Bcc: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Bcc: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Cc l ->
           w "Cc: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Cc: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ReturnPath (Some m) ->
           w  "Return-Path: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Return-Path: ")
                             $ Address.E.w_mailbox m
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ReturnPath None ->
           w "Return-Path: < >" $ w_crlf
         | `ResentTo l ->
           w "Resent-To: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-To: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Subject p ->
           w "Subject:"
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Subject:")
                             $ Address.E.w_phrase p
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `To l ->
           w "To: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "To: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `References l ->
           let w_data = function
             | `Phrase p -> Address.E.w_phrase p
             | `MsgID m -> MsgID.E.w m
           in
           w "References: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "References: ")
                             $ w_lst w_space w_data l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentSender p ->
           w "Resent-Sender: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Sender: ")
                             $ Address.E.w_person p
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Field (key, value) ->
           w key $ w ":"
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length key + 2)
                             $ Address.E.w_phrase value
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Date d ->
           w "Date: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Date: ")
                             $ Date.E.w d
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `InReplyTo l ->
           let w_data = function
             | `Phrase p -> Address.E.w_phrase p
             | `MsgID m -> MsgID.E.w m
           in
           w "In-Reply-To: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "In-Reply-To: ")
                             $ w_lst w_space w_data l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Received (l, Some date) ->
           let w_data = function
             | `Word word -> Address.E.w_word word
             | `Domain domain -> Address.E.w_domain domain
             | `Mailbox mailbox -> Address.E.w_mailbox mailbox
           in
           w "Received: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Received: ")
                             $ w_hovbox 1 $ w_lst w_space w_data l $ w_close_box
                             $ w_hovbox 1 $ w_string ";" $ w_space $ Date.E.w date $ w_close_box
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Received (l, None) ->
           let w_data = function
             | `Word word -> Address.E.w_word word
             | `Domain domain -> Address.E.w_domain domain
             | `Mailbox mailbox -> Address.E.w_mailbox mailbox
           in
           w "Received: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Received: ")
                             $ w_hovbox 1 $ w_lst w_space w_data l $ w_close_box
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `MessageID m ->
           w "Message-ID: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Message-ID: ")
                             $ MsgID.E.w m
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Comments p ->
           w "Comments:"
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Comments: ")
                             $ Address.E.w_phrase p
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentBcc l ->
           w "Resent-Bcc: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Bcc: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `From l ->
           w "From: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "From: ")
                             $ w_lst (w_string "," $ w_space) Address.E.w_person l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentFrom l ->
           w "Resent-From: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-From: ")
                             $ w_lst (w_string "," $ w_space) Address.E.w_person l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Sender p ->
           w "Sender: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Sender: ")
                             $ Address.E.w_person p
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentReplyTo l ->
           w "Resent-Reply-To: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Reply-To: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Unsafe (key, value) ->
           w key $ w ":"
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length key + 2)
                             $ Address.E.w_phrase value
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `Keywords l ->
           w "Keywords:"
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Keywords: ")
                             $ w_lst (w_string "," $ w_space) Address.E.w_phrase l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ReplyTo l ->
           w "Reply-To: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Reply-To: ")
                             $ Address.List.E.w l
                             $ w_close_box) (unlift k))
           $ w_crlf
         | `ResentDate d ->
           w "Resent-Date: "
           $ Wrap.lift
           $ Wrap.(fun k -> (w_hovbox (String.length "Resent-Date: ")
                             $ Date.E.w d
                             $ w_close_box) (unlift k))
           $ w_crlf)
        fields
  end

  let w = Internal.w_unstrict_header

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
      Internal.w_unstrict_header (to_field t) ok
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string (s ^ "\r\n\r\n"))
let to_string t = Buffer.contents @@ E.to_buffer t (Encoder.make ())

let equal = (=)

let pp fmt _ = Format.fprintf fmt "#header"
