type field = Rfc5322.resent

type resent =
  { date     : MrMime_date.date option
  ; from     : MrMime_address.mailbox list
  ; sender   : MrMime_address.mailbox option
  ; to'      : MrMime_address.address list
  ; cc       : MrMime_address.address list
  ; bcc      : MrMime_address.address list
  ; msg_id   : MrMime_msgID.msg_id option
  ; reply_to : MrMime_address.address list }

let default =
  { date     = None
  ; from     = []
  ; sender   = None
  ; to'      = []
  ; cc       = []
  ; bcc      = []
  ; msg_id   = None
  ; reply_to = [] }

(* convenience alias *)
module Date    = MrMime_date
module Address = MrMime_address
module MsgID   = MrMime_msgID

let pp fmt _ = Format.pp_print_string fmt "#resent"

module Encoder =
struct
  open Encoder

  let w_lst w_sep w_data l =
      let rec aux = function
      | [] -> noop
      | [ x ] -> w_data x
      | x :: r -> w_data x $ w_sep $ aux r
    in aux l

  let w_crlf k e = string "\r\n" k e

  let w_field = function
    | `ResentCc l ->
      string "Resent-Cc: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `ResentMessageID m ->
      string "Resent-Message-ID: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ MsgID.Encoder.w_msg_id m $ close_box) (unlift k))))
      $ w_crlf
    | `ResentSender p ->
      string "Resent-Sender: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_mailbox p $ close_box) (unlift k))))
      $ w_crlf
    | `ResentBcc l ->
      string "Resent-Bcc: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `ResentFrom l ->
      string "Resent-From: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst (string "," $ space) Address.Encoder.w_mailbox l $ close_box) (unlift k))))
      $ w_crlf
    | `ResentReplyTo l ->
      string "Resent-Reply-To: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf
    | `ResentDate d ->
      string "Resent-Date: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Date.Encoder.w_date d $ close_box) (unlift k))))
      $ w_crlf
    | `ResentTo l ->
      string "Resent-To: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_addresses l $ close_box) (unlift k))))
      $ w_crlf

  let w_resent { date; from; sender; to'; cc; bcc; msg_id; reply_to; } =
    (match date with Some v -> w_field (`ResentDate v) | None -> noop)
    $ (match from with [] -> noop | v -> w_field (`ResentFrom v))
    $ (match sender with Some v -> w_field (`ResentSender v) | None -> noop)
    $ (match to' with [] -> noop | v -> w_field (`ResentTo v))
    $ (match cc with [] -> noop | v -> w_field (`ResentCc v))
    $ (match bcc with [] -> noop | v -> w_field (`ResentBcc v))
    $ (match msg_id with Some v -> w_field (`ResentMessageID v) | None -> noop)
    $ (match reply_to with [] -> noop | v -> w_field (`ResentReplyTo v))
end

let decoder (fields : [> field ] list) =
  { Parser.f = fun i s _fail succ ->
    let rec catch garbage acc lst = match lst, acc with
      | `ResentDate date :: r, ([] as r')
      | `ResentDate date :: r, (({ date = Some _; _ } :: _)  as r') ->
        catch garbage ({ default with date = Some date } :: r') r
      | `ResentDate date :: r, x :: r' ->
        catch garbage ({ x with date = Some date } :: r') r
      | `ResentFrom from :: r, ([] as r')
      | `ResentFrom from :: r, (({ from = _ :: _; _ } :: _) as r') ->
        catch garbage ({ default with from = from } :: r') r
      | `ResentFrom from :: r, x :: r' ->
        catch garbage ({ x with from = x.from @ from } :: r') r
      | `ResentSender v :: r, x :: r' ->
        catch garbage ({ x with sender = Some v } :: r') r
      | `ResentTo v :: r, x :: r' ->
        catch garbage ({ x with to' = x.to' @ v } :: r') r
      | `ResentCc v :: r, x :: r' ->
        catch garbage ({ x with cc = x.cc @ v } :: r') r
      | `ResentBcc v :: r, x :: r' ->
        catch garbage ({ x with bcc = x.bcc @ v } :: r') r
      | `ResentMessageID v :: r, x :: r' ->
        catch garbage ({ x with msg_id = Some v } :: r') r
      | `ResentReplyTo v :: r, x :: r' ->
        catch garbage ({ x with reply_to = x.reply_to @ v } :: r') r
      | field :: r, acc ->
        catch (field :: garbage) acc r
      | [], acc -> acc, List.rev garbage (* keep the order *)
    in

    succ i s (catch [] [] fields) }

let equal = (=)
