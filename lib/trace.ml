type local  = Rfc822.local
type domain = Rfc5322.domain
type word   = Rfc822.word
type field  = Rfc5322.trace

type received =
  [ `Addr   of local * (domain * domain list)
  | `Domain of domain
  | `Word   of word ]

type trace =
  { trace    : (local * (domain * domain list)) option
  ; received : (received list * Date.date option) list }

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

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

let pp fmt = function
  | { trace = Some p
    ; received = r } ->
    pp fmt "@[<hov>Return-Path = %a@]@\n& %a"
      pp_path p
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n& ") pp_received) r
  | { trace = None
    ; received = r } ->
    pp fmt "%a"
      (pp_lst ~sep:(fun fmt () -> pp fmt "@\n& ") pp_received) r

module Encoder =
struct
  open Encoder

  let w_crlf k e = string "\r\n" k e

  let rec w_lst w_sep w_data l =
    let open Wrap in
      let rec aux = function
      | [] -> noop
      | [ x ] -> w_data x
      | x :: r -> w_data x $ w_sep $ aux r
    in aux l


  let w_field = function
    | `Received (l, Some date) ->
      let w_data = function
        | `Word word -> Address.Encoder.w_word word
        | `Domain domain -> Address.Encoder.w_domain domain
        | `Addr addr -> Address.Encoder.w_mailbox' addr
      in
      string "Received: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst space w_data l $ close_box
                               $ hovbox 0 $ string ";" $ space $ Date.Encoder.w_date date $ close_box) (unlift k))))
      $ w_crlf
    | `Received (l, None) ->
      let w_data = function
        | `Word word -> Address.Encoder.w_word word
        | `Domain domain -> Address.Encoder.w_domain domain
        | `Addr addr -> Address.Encoder.w_mailbox' addr
      in
      string "Received: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ w_lst space w_data l $ close_box) (unlift k))))
      $ w_crlf
    | `ReturnPath (Some m) ->
      string  "Return-Path: "
      $ (fun k -> Wrap.(lift ((hovbox 0 $ Address.Encoder.w_mailbox' m $ close_box) (unlift k))))
      $ w_crlf
    | `ReturnPath None ->
      string "Return-Path: < >" $ w_crlf

  let w_trace { trace; received; } =
    w_field (`ReturnPath trace)
    $ List.fold_right (fun x acc -> w_field (`Received x) $ acc) received noop
end

let decoder (fields : [> field ] list) =
  { Parser.f = fun i s fail succ ->
    let rec catch garbage acc = function
      | `Trace (trace, received) :: r ->
        catch garbage ({ trace; received; } :: acc) r
      | field :: r ->
        catch (field :: garbage) acc r
      | [] -> acc, List.rev garbage (* keep the order *)
    in

    succ i s (catch [] [] fields) }

let equal = (=)
