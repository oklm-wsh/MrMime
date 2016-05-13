open BasePP

type t =
  { date     : Date.t
  ; from     : Address.person list
  ; sender   : Address.person option
  ; target   : Address.List.t option
  ; cc       : Address.List.t option
  ; bcc      : Address.List.t option
  ; msg_id   : MsgID.t option
  ; reply_to : Address.List.t option }

type field =
  [ `ResentDate of Date.t
  | `ResentFrom of Address.person list
  | `ResentSender of Address.person
  | `ResentTo of Address.List.t
  | `ResentCc of Address.List.t
  | `ResentBcc of Address.List.t
  | `ResentMessageID of MsgID.t
  | `ResentReplyTo of Address.List.t ]

let field_of_lexer : Rfc5322.resent -> field = function
  | `ResentDate d -> `ResentDate (Date.of_lexer d)
  | `ResentFrom l -> `ResentFrom (List.map Address.person_of_lexer l)
  | `ResentSender p -> `ResentSender (Address.person_of_lexer p)
  | `ResentTo l -> `ResentTo (Address.List.of_lexer l)
  | `ResentCc l -> `ResentCc (Address.List.of_lexer l)
  | `ResentBcc l -> `ResentBcc (Address.List.of_lexer l)
  | `ResentMessageID m -> `ResentMessageID (MsgID.of_lexer m)
  | `ResentReplyTo l -> `ResentReplyTo (Address.List.of_lexer l)

let of_lexer fields p state =
  let date     = ref None in
  let from     = ref None in
  let sender   = ref None in
  let target   = ref None in
  let cc       = ref None in
  let bcc      = ref None in
  let msg_id   = ref None in
  let reply_to = ref None in

  let sanitize fields =
    match !date, !from with
    | Some date, Some from ->
      p (Some { date; from
              ; sender   = !sender
              ; target   = !target
              ; cc       = !cc
              ; bcc      = !bcc
              ; msg_id   = !msg_id
              ; reply_to = !reply_to })
        fields state
    | _ -> p None fields state
  in

  let rec loop garbage fields = match fields with
    | [] -> sanitize (List.rev garbage)
    | field :: rest ->
      match field with
      | `ResentDate d ->
        (match !date with
         | None   -> date := Some (Date.of_lexer d); loop garbage rest
         | Some _ -> sanitize (List.rev (field :: garbage) @ rest))
      | `ResentFrom f ->
        (match !from with
         | None   -> from := Some (List.map Address.person_of_lexer f); loop garbage rest
         | Some _ -> sanitize (List.rev (field :: garbage) @ rest))
      | `ResentSender p ->
        (match !sender with
         | None   -> sender := Some (Address.person_of_lexer p); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ResentTo t ->
        (match !target with
         | None   -> target := Some (Address.List.of_lexer t); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ResentCc c ->
        (match !cc with
         | None   -> cc := Some (Address.List.of_lexer c); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ResentBcc c ->
        (match !bcc with
         | None   -> bcc := Some (Address.List.of_lexer c); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ResentMessageID m ->
        (match !msg_id with
         | None   -> msg_id := Some (MsgID.of_lexer m); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | `ResentReplyTo l ->
        (match !reply_to with
         | None   -> reply_to := Some (Address.List.of_lexer l); loop garbage rest
         | Some _ -> loop (field :: garbage) rest)
      | field -> loop (field :: garbage) rest
  in

  loop [] fields

let pp fmt { date; from; sender; target; cc; bcc; msg_id; } =
  let pp_field_opt fmt field_name pp_field field_opt =
    match field_opt with
    | Some field -> p fmt "%s: %a\r\n" field_name pp_field field
    | None       -> ()
  in

  p fmt "Resent-Date: %a\r\n" Date.pp date;
  p fmt "Resent-From: %a\r\n" (pp_list ~sep:", " Address.pp_person) from;
  pp_field_opt fmt "Resent-Sender" Address.pp_person sender;
  pp_field_opt fmt "Resent-To" Address.List.pp target;
  pp_field_opt fmt "Resent-Cc" Address.List.pp cc;
  pp_field_opt fmt "Resent-Bcc" Address.List.pp bcc;
  pp_field_opt fmt "Resent-Message-ID" MsgID.pp msg_id;
