open Base

type t =
  { date   : Date.t
  ; from   : Address.person list
  ; sender : Address.person option
  ; target : Address.List.t option
  ; cc     : Address.List.t option
  ; bcc    : Address.List.t option
  ; msg_id : MsgID.t option }

let of_lexer k l =
  let date   = ref None in
  let from   = ref None in
  let sender = ref None in
  let target = ref None in
  let cc     = ref None in
  let bcc    = ref None in
  let msg_id = ref None in

  let sanitize fields =
    match !date, !from with
    | Some date, Some from ->
      k (Some { date; from
              ; sender = !sender
              ; target = !target
              ; cc     = !cc
              ; bcc    = !bcc
              ; msg_id = !msg_id })
        fields
    | _ -> k None fields
  in

  let rec loop i l = match l with
    | [] -> sanitize (List.rev i)
    | x :: rest ->
      match x with
      | `ResentDate d ->
        (match !date with
         | None   -> date := Some (Date.of_lexer d); loop i rest
         | Some _ -> sanitize (List.rev (x :: i) @ l))
      | `ResentFrom f ->
        (match !from with
         | None   -> from := Some (List.map Address.person_of_lexer f); loop i rest
         | Some _ -> sanitize (List.rev (x :: i) @ l))
      | `ResentSender p ->
        (match !sender with
         | None   -> sender := Some (Address.person_of_lexer p); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ResentTo t ->
        (match !target with
         | None   -> target := Some (Address.List.of_lexer t); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ResentCc c ->
        (match !cc with
         | None   -> cc := Some (Address.List.of_lexer c); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ResentBcc c ->
        (match !bcc with
         | None   -> bcc := Some (Address.List.of_lexer c); loop i rest
         | Some _ -> loop (x :: i) rest)
      | `ResentMessageID m ->
        (match !msg_id with
         | None   -> msg_id := Some (MsgID.of_lexer m); loop i rest
         | Some _ -> loop (x :: i) rest)
      | field -> loop (field :: i) rest
  in

  loop [] l

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
