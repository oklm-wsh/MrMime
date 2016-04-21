(* See RFC 6854 § 2.1

   from            = "From:" (mailbox-list / address-list) CRLF
   sender          = "Sender:" (mailbox / address) CRLF
   reply-to        = "Reply-To:" address-list CRLF
*)

let p_from p =
  Lexer.p_try_rule p
    (Rfc5322.p_address_list p)
    (Rfc5322.p_mailbox_list
       (fun data state -> `Ok (List.map (fun x -> `Person x) data, state)))

let p_sender p =
  Lexer.p_try_rule p
    (Rfc5322.p_address p)
    (Rfc5322.p_mailbox (fun data state -> `Ok (`Person data, state)))

let p_reply_to = Rfc5322.p_address_list

(* See RFC 6854 § 2.2:

   resent-from     = "Resent-From:" (mailbox-list / address-list) CRLF
   resent-sender   = "Resent-Sender:" (mailbox / address) CRLF
*)

let p_resent_from p =
  Lexer.p_try_rule p
    (Rfc5322.p_address_list p)
    (Rfc5322.p_mailbox_list
       (fun data state -> `Ok (List.map (fun x -> `Person x) data, state)))

let p_resent_sender p =
  Lexer.p_try_rule p
    (Rfc5322.p_address p)
    (Rfc5322.p_mailbox (fun data state -> `Ok (`Person data, state)))
