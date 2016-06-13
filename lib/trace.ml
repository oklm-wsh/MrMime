(* TODO: implement RFC 5321 § 4.4 *)

type word = [ `Word of Rfc5322.word ]

type received =
  [ word
  | `Domain  of Address.domain
  | `Mailbox of Address.mailbox ]

type t =
  { received : (received list * Date.t option) list
  ; path     : Address.mailbox option }

type field =
  [ `Received of received list * Date.t option
  | `ReturnPath of Address.mailbox option ]

let field_of_lexer : Rfc5322.trace -> field = function
  | `Received (l, d) ->
    let l = List.map (function
                      | `Domain d  -> `Domain (Address.D.domain_of_lexer d)
                      | `Mailbox a -> `Mailbox (Address.D.mailbox_of_lexer a)
                      | #word as x -> x) l in
    let d = Option.bind Date.D.of_lexer d in
    `Received (l, d)
  | `ReturnPath a -> `ReturnPath (Option.bind Address.D.mailbox_of_lexer a)

let to_field trace =
  [`ReturnPath trace.path ]
  @ (List.map (fun x -> `Received x) trace.received)

module D =
struct
  let of_lexer fields p state =
    let received = ref [] in
    let path     = ref None in

    let sanitize fields =
      match !received, !path with
      | [], None -> p None fields state
      | _ -> p (Some { received = List.rev !received
                     ; path = Option.value ~default:None !path; }) fields state
    in

    let rec loop garbage fields = match fields with
      | [] -> sanitize (List.rev garbage)
      | field :: rest ->
        match field with
        | `Received (l, d) ->
          let l = List.map (function
                            | `Domain d  -> `Domain (Address.D.domain_of_lexer d)
                            | `Mailbox a -> `Mailbox (Address.D.mailbox_of_lexer a)
                            | #word as x -> x) l in
          let d = Option.bind Date.D.of_lexer d in
          received := (l, d) :: !received; loop garbage rest
        | `ReturnPath a ->
          (match !path with
           | None   ->
             path := Some (Option.bind Address.D.mailbox_of_lexer a);
             loop garbage rest
           | Some _ -> sanitize (List.rev (field :: garbage) @ rest))
        | field -> loop (field :: garbage) rest
    in

    loop [] fields
end
