type t =
  { date     : Date.t
  ; from     : Address.person list
  ; sender   : Address.person option
  ; target   : Address.t list option
  ; cc       : Address.t list option
  ; bcc      : Address.t list option
  ; msg_id   : MsgID.t option
  ; reply_to : Address.t list option }

type field =
  [ `ResentDate      of Date.t
  | `ResentFrom      of Address.person list
  | `ResentSender    of Address.person
  | `ResentTo        of Address.t list
  | `ResentCc        of Address.t list
  | `ResentBcc       of Address.t list
  | `ResentMessageID of MsgID.t
  | `ResentReplyTo   of Address.t list ]

let field_of_lexer : Rfc5322.resent -> field = function
  | `ResentDate d      -> `ResentDate (Date.D.of_lexer d)
  | `ResentFrom l      -> `ResentFrom (List.map Address.D.person_of_lexer l)
  | `ResentSender p    -> `ResentSender (Address.D.person_of_lexer p)
  | `ResentTo l        -> `ResentTo (Address.List.D.of_lexer l)
  | `ResentCc l        -> `ResentCc (Address.List.D.of_lexer l)
  | `ResentBcc l       -> `ResentBcc (Address.List.D.of_lexer l)
  | `ResentMessageID m -> `ResentMessageID (MsgID.D.of_lexer m)
  | `ResentReplyTo l   -> `ResentReplyTo (Address.List.D.of_lexer l)

let to_field resent =
  let ( >>= ) o f = match o with Some x -> Some (f x) | None -> None in
  let ( @:@ ) o r = match o with Some x -> x :: r | None -> r in

  (resent.sender       >>= fun p -> `ResentSender p)
  @:@ (resent.target   >>= fun l -> `ResentTo l)
  @:@ (resent.cc       >>= fun l -> `ResentCc l)
  @:@ (resent.bcc      >>= fun l -> `ResentBcc l)
  @:@ (resent.msg_id   >>= fun m -> `ResentMessageID m)
  @:@ (resent.reply_to >>= fun l -> `ResentReplyTo l)
  @:@ (`ResentDate resent.date) :: (`ResentFrom resent.from) :: []

module D =
struct
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
           | None   -> date := Some (Date.D.of_lexer d); loop garbage rest
           | Some _ -> sanitize (List.rev (field :: garbage) @ rest))
        | `ResentFrom f ->
          (match !from with
           | None   -> from := Some (List.map Address.D.person_of_lexer f); loop garbage rest
           | Some _ -> sanitize (List.rev (field :: garbage) @ rest))
        | `ResentSender p ->
          (match !sender with
           | None   -> sender := Some (Address.D.person_of_lexer p); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | `ResentTo t ->
          (match !target with
           | None   -> target := Some (Address.List.D.of_lexer t); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | `ResentCc c ->
          (match !cc with
           | None   -> cc := Some (Address.List.D.of_lexer c); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | `ResentBcc c ->
          (match !bcc with
           | None   -> bcc := Some (Address.List.D.of_lexer c); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | `ResentMessageID m ->
          (match !msg_id with
           | None   -> msg_id := Some (MsgID.D.of_lexer m); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | `ResentReplyTo l ->
          (match !reply_to with
           | None   -> reply_to := Some (Address.List.D.of_lexer l); loop garbage rest
           | Some _ -> loop (field :: garbage) rest)
        | field -> loop (field :: garbage) rest
    in

    loop [] fields
end

module E =
struct

end
