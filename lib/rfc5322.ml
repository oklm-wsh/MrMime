let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

type phrase =
  [ `Dot
  | `Word of Rfc822.word
  | `Encoded of (string * Rfc2047.raw) ] list
type domain =
  [ `Domain of string list
  | `Literal of Rfc5321.literal_domain ]
type mailbox =
  { name    : phrase option
  ; local   : Rfc822.local
  ; domain  : domain * domain list }
type group =
  { name    : phrase
  ; mailbox : mailbox list }
type address = [ `Group of group | `Mailbox of mailbox ]
type month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec
type day =
  | Mon | Tue | Wed
  | Thu | Fri | Sat
  | Sun
type zone =
  | UT  | GMT
  | EST | EDT
  | CST | CDT
  | MST | MDT
  | PST | PDT
  | Military_zone of char
  | TZ of int
type date =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; zone : zone }
type unstructured =
  [ `Text of string | `CR of int | `LF of int | `CRLF | `WSP
  | `Encoded of (string * Rfc2047.raw) ] list
type phrase_or_msg_id =
  [ `Phrase of phrase | `MsgID of Rfc822.msg_id ]
type resent =
  [ `ResentDate      of date
  | `ResentFrom      of mailbox list
  | `ResentSender    of mailbox
  | `ResentTo        of address list
  | `ResentCc        of address list
  | `ResentBcc       of address list
  | `ResentMessageID of Rfc822.msg_id
  | `ResentReplyTo   of address list ]
type trace =
  [ `Trace of ((Rfc822.local * (domain * domain list)) option
               * ([ `Addr   of Rfc822.local * (domain * domain list)
                  | `Domain of domain
                  | `Word   of Rfc822.word ] list * date option) list) ]
type field_header =
  [ `Date       of date
  | `From       of mailbox list
  | `Sender     of mailbox
  | `ReplyTo    of address list
  | `To         of address list
  | `Cc         of address list
  | `Bcc        of address list
  | `MessageID  of Rfc822.msg_id
  | `InReplyTo  of phrase_or_msg_id list
  | `References of phrase_or_msg_id list
  | `Subject    of unstructured
  | `Comments   of unstructured
  | `Keywords   of phrase list
  | `Field      of string * unstructured
  | `Unsafe     of string * unstructured ]
type skip =
  [ `Skip       of string ]
type field =
  [ field_header | resent | trace | skip ]

open Parser
open Parser.Convenience

type err += Incomplete_address_literal

let domain_literal =
  (option () Rfc822.cfws)
  *> char '['
  *> (many ((option (false, false, false) Rfc822.fws)
            *> ((Rfc6532.str Rfc822.is_dtext) <|> (Rfc822.quoted_pair >>| String.make 1)))
      >>| String.concat "")
  <* (option (false, false, false) Rfc822.fws)
  <* char ']'
  <* (option () Rfc822.cfws)
  >>= fun content ->
    { f = fun i s fail succ ->
      let b = Input.create_by ~proof:(Input.proof i) (String.length content) in
      Input.write b (Internal_buffer.from_string ~proof:(Input.proof b) content) 0 (String.length content);

      let compute = function
        | Read _ -> fail i s [] Incomplete_address_literal
        | Fail (_, err) -> fail i s [] err
        | Done v -> succ i s v
      in

      compute @@ only b Rfc5321.address_literal }

let domain =
  (Rfc822.obs_domain >>| fun domain -> `Domain domain)
  <|> (domain_literal >>| fun literal -> `Literal literal)
  <|> (Rfc822.dot_atom >>| fun domain -> `Domain domain)

let addr_spec =
  Rfc822.local_part
  >>= fun local -> char '@'
  *> domain
  >>= fun domain -> return (local, domain)

let word' =
  (option () Rfc822.cfws
   *> (Rfc2047.inline_encoded_string >>| fun x -> `Encoded x)
   <* option () Rfc822.cfws)
  <|> (Rfc822.word >>| fun x -> `Word x)

let obs_phrase =
  word'
  >>= fun first ->
    fix (fun m -> (lift2 (function
                          | (`Dot | `Word _ | `Encoded _) as x -> fun r -> x :: r
                          | `CFWS -> fun r -> r)
                   (word'
                    <|> (char '.' >>| fun _ -> `Dot)
                    <|> (Rfc822.cfws >>| fun () -> `CFWS))
                   m)
                  <|> return [])
  >>| fun rest -> first :: rest

let phrase = obs_phrase <|> (one word')
let display_name = phrase

let obs_domain_list =
  let first =
    fix (fun m -> (lift2 (fun _ _ -> ()) (Rfc822.cfws
                                          <|> (char ',' >>| fun _ -> ())) m)
                  <|> return ())
  in
  let rest =
    fix (fun m -> (lift2 (function `Sep -> fun r -> r
                                 | `Domain x -> fun r -> x :: r)
                         (char ','
                          *> (option () Rfc822.cfws)
                          *> (option `Sep (char '@'
                                           *> domain
                                           >>| fun x -> `Domain x)))
                         m)
                  <|> return [])
  in
  first *> char '@' *> domain
  >>= fun x -> rest
  >>| fun r -> x :: r

let obs_route = obs_domain_list <* char ':'

let obs_angle_addr =
  (option () Rfc822.cfws)
  *> char '<'
  *> obs_route
  >>= fun domains -> addr_spec
  >>= fun (local, domain) -> char '>'
  *> (option () Rfc822.cfws)
  >>| fun () -> (local, (domain, domains))

let angle_addr =
  obs_angle_addr
  <|> ((option () Rfc822.cfws)
       *> char '<'
       *> addr_spec
       >>= fun (local, domain) -> char '>'
       *> (option () Rfc822.cfws)
       >>| fun _ -> (local, (domain, [])))

let name_addr =
  (option None (display_name >>| fun x -> Some x))
  >>= fun name -> angle_addr
  >>| fun addr -> (name, addr)

let mailbox =
  (name_addr
   <|> (addr_spec >>| fun (local, domain) -> (None, (local, (domain, [])))))
  >>| (fun (name, (local, domain)) -> { name; local; domain; })

let obs_mbox_list =
  let many' p =
    fix (fun m -> (lift2 (fun _ _ -> ()) p m) <|> return ())
  in
  let rest =
    fix (fun m -> (lift2 (function `Mailbox x -> fun r -> x :: r
                                 | `Sep -> fun r -> r)
                         (char ',' *> (option `Sep ((mailbox >>| fun m -> `Mailbox m)
                                       <|> (Rfc822.cfws >>| fun () -> `Sep))))
                         m)
                  <|> return [])
  in
  (many' ((option () Rfc822.cfws) *> char ','))
  *> mailbox
  >>= fun x -> rest
  >>| fun r -> x :: r

let obs_group_list =
  let many' p =
    fix (fun m -> (lift2 (fun _ _ -> ()) p m) <|> return ())
  in
  let one' p =
    lift2 (fun _ _ -> ()) p (many' p)
  in
  one' ((option () Rfc822.cfws) *> char ',') *> (option () Rfc822.cfws)

let mailbox_list =
  obs_mbox_list
  <|> (mailbox >>= fun x -> (many (char ',' *> mailbox)) >>| fun r -> x :: r)

let group_list =
  mailbox_list
  <|> (obs_group_list >>| fun () -> [])
  <|> (Rfc822.cfws >>| fun () -> [])

let group =
  display_name
  >>= fun name -> char ':'
  *> (option [] group_list <?> "group-list")
  >>= fun lst -> char ';'
  *> (option () Rfc822.cfws)
  >>| fun _ -> { name; mailbox = lst; }

let address =
  (group >>| fun g -> `Group g)
  <|> (mailbox >>| fun m -> `Mailbox m)

let obs_addr_list =
  let many' p =
    fix (fun m -> (lift2 (fun _ _ -> ()) p m) <|> return ())
  in
  let rest =
    fix (fun m -> (lift2 (function `Addr x -> fun r -> x :: r
                                 | `Sep -> fun r -> r)
                         (char ',' *> (option `Sep ((address >>| fun a -> `Addr a)
                                       <|> (Rfc822.cfws >>| fun () -> `Sep))))
                         m)
                  <|> return [])
  in
  (many' ((option () Rfc822.cfws) *> char ','))
  *> address
  >>= fun x -> rest
  >>| fun r -> x :: r

let address_list =
  obs_addr_list
  <|> (address >>= fun x -> (many (char ',' *> address)) >>| fun r -> x :: r)

let is_digit = function '0' .. '9' -> true | _ -> false

let obs_hour =
  (option () Rfc822.cfws)
  *> repeat (Some 2) (Some 2) is_digit
  <* (option () Rfc822.cfws)
  >>| int_of_string

let obs_minute =
  (option () Rfc822.cfws)
  *> repeat (Some 2) (Some 2) is_digit
  >>| int_of_string
let obs_second =
  (option () Rfc822.cfws)
  *> repeat (Some 2) (Some 2) is_digit
  >>| int_of_string

let hour   = obs_hour   <|> (repeat (Some 2) (Some 2) is_digit >>| int_of_string)
let minute = obs_minute <|> (repeat (Some 2) (Some 2) is_digit >>| int_of_string)
let second = obs_second <|> (repeat (Some 2) (Some 2) is_digit >>| int_of_string)

let obs_year =
  (option () Rfc822.cfws)
  *> (repeat (Some 2) None is_digit)
  <* (option () Rfc822.cfws)
  >>| int_of_string

let year =
  (Rfc822.fws
   *> (repeat (Some 4) None is_digit)
   <* Rfc822.fws
   >>| int_of_string)
  <|> obs_year

let obs_day =
  (option () Rfc822.cfws)
  *> (repeat (Some 1) (Some 2) is_digit)
  <* (option () Rfc822.cfws)
  >>| int_of_string

let day =
  obs_day
  <|> ((option (false, false, false) Rfc822.fws)
       *> (repeat (Some 1) (Some 2) is_digit)
       <* Rfc822.fws
       >>| int_of_string)

let month =
  let string s = string (fun x -> x) s in
  (string "Jan" *> return Jan)
  <|> (string "Feb" *> return Feb)
  <|> (string "Mar" *> return Mar)
  <|> (string "Apr" *> return Apr)
  <|> (string "May" *> return May)
  <|> (string "Jun" *> return Jun)
  <|> (string "Jul" *> return Jul)
  <|> (string "Aug" *> return Aug)
  <|> (string "Sep" *> return Sep)
  <|> (string "Oct" *> return Oct)
  <|> (string "Nov" *> return Nov)
  <|> (string "Dec" *> return Dec)

let day_name =
  let string s = string (fun x -> x) s in
  (string "Mon" *> return Mon)
  <|> (string "Tue" *> return Tue)
  <|> (string "Wed" *> return Wed)
  <|> (string "Thu" *> return Thu)
  <|> (string "Fri" *> return Fri)
  <|> (string "Sat" *> return Sat)
  <|> (string "Sun" *> return Sun)

let obs_day_of_week =
  (option () Rfc822.cfws)
  *> day_name
  <* (option () Rfc822.cfws)

let day_of_week =
  obs_day_of_week
  <|> ((option (false, false, false) Rfc822.fws) *> day_name)

let date =
  lift3 (fun day month year -> (day, month, year)) day month year

let time_of_day =
  hour
  >>= fun hour -> char ':' *> minute
  >>= fun minute -> option None ((option () Rfc822.cfws)
                                 *> char ':'
                                 *> second
                                 >>| fun second -> Some second)
  >>| fun second -> (hour, minute, second)

let is_military_zone = function
  | '\065' .. '\073'
  | '\075' .. '\090'
  | '\097' .. '\105'
  | '\107' .. '\122' -> true
  | _ -> false

let obs_zone =
  let string s = string (fun x -> x) s in
  (string "UT" *> return UT)
  <|> (string "GMT" *> return GMT)
  <|> (string "EST" *> return EST)
  <|> (string "EDT" *> return EDT)
  <|> (string "CST" *> return CST)
  <|> (string "CDT" *> return CDT)
  <|> (string "MST" *> return MST)
  <|> (string "MDT" *> return MDT)
  <|> (string "PST" *> return PST)
  <|> (string "PDT" *> return PDT)
  <|> (satisfy is_military_zone >>= fun z -> return (Military_zone z))

let zone =
  (Rfc822.fws *> satisfy (function '+' | '-' -> true | _ -> false)
   >>= fun sign -> repeat (Some 4) (Some 4) is_digit
   >>| fun zone ->
     if sign = '-'
     then TZ (- (int_of_string zone))
     else TZ (int_of_string zone))
  <|> ((option () Rfc822.cfws) *> obs_zone)

let time = lift2 (fun time zone -> (time, zone)) time_of_day zone

let date_time =
  lift3
    (fun day date (time, zone) -> { day; date; time; zone; })
    (option None (day_of_week >>= fun day -> char ',' *> return (Some day)))
    date time
  <* (option () Rfc822.cfws)

let is_obs_utext = function
  | '\000' -> true
  | c -> Rfc822.is_obs_no_ws_ctl c || Rfc822.is_vchar c

let obs_unstruct : unstructured t =
  let many' p = fix (fun m -> (lift2 (fun _ r -> r + 1) p m) <|> return 0) in

  let word =
    (Rfc2047.inline_encoded_string >>| fun e -> `Encoded e)
    <|> (Rfc6532.str is_obs_utext >>| fun e -> `Text e) in

  let safe_lfcr =
    many' (char '\n') >>= fun lf ->
    many' (char '\r') >>= fun cr ->
    peek_chr >>= fun chr -> match lf, cr, chr with
    | 0, 0, _ -> return []
    | n, 0, _ -> return [`LF n]
    | n, 1, Some '\n' ->
      { f = fun i s fail succ ->
          Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) "\r");

          succ i s () } *> return (if n <> 0 then [`LF n] else [])
    | n, m, Some '\n' ->
      { f = fun i s fail succ ->
          Input.rollback i (Internal_buffer.from_string ~proof:(Input.proof i) "\r");

          succ i s () } *> return (if n <> 0 then [`LF n; `CR (m - 1)] else [`CR (m - 1)])
    | n, m, _ -> return [`LF n; `CR 128]
  in

  many
  ((safe_lfcr >>= fun pre -> many (word >>= fun word -> safe_lfcr >>| fun rst -> word :: rst) >>| fun rst -> List.concat (pre :: rst))
   <|> (Rfc822.fws >>| function
        | true,  true,  true  -> [`WSP; `CRLF; `WSP]
        | false, true,  true  -> [`CRLF; `WSP]
        | true,  true,  false -> [`WSP; `CRLF]
        | false, true,  false -> [`CRLF]
        | true,  false, true  -> [`WSP; `WSP]
        | true,  false, false
        | false, false, true  -> [`WSP]
        | false, false, false -> []))
  >>| List.concat

let make n f =
  let rec aux acc = function
    | 0 -> List.rev acc
    | n -> aux (f n :: acc) (n - 1)
  in
  aux [] n

let unstructured =
  let many' p = fix (fun m -> (lift2 (fun _ r -> r + 1) p m) <|> return 0) in
  obs_unstruct
  <|> (many (option (false, false, false) Rfc822.fws
       >>= fun (has_wsp, has_crlf, has_wsp') -> Rfc6532.str Rfc822.is_vchar
       >>| fun text -> match has_wsp, has_crlf, has_wsp' with
                       | true,  true,  true  -> [`WSP; `CRLF; `WSP; `Text text]
                       | false, true,  true  -> [`CRLF; `WSP; `Text text]
                       | true,  true,  false -> [`WSP; `CRLF; `Text text]
                       | false, true,  false -> [`CRLF; `Text text]
                       | true,  false, true  -> [`WSP; `WSP; `Text text]
                       | true,  false, false
                       | false, false, true  -> [`WSP; `Text text]
                       | false, false, false -> [`Text text])
       >>= fun pre -> many' (char '\x09' <|> char '\x20')
       >>| fun n -> List.concat pre @ make n (fun _ -> `WSP))


let phrase_or_msg_id =
  many ((phrase >>| fun v -> `Phrase v) <|> (Rfc822.msg_id >>| fun v -> `MsgID v))

let obs_phrase_list =
  (option [] (phrase <|> (Rfc822.cfws *> return [])))
  >>= fun pre -> many (char ',' *> (option [] (phrase <|> (Rfc822.cfws *> return []))))
  >>| fun rst -> (pre :: rst)

let keywords =
  let sep s p = fix (fun m -> lift2 (fun x r -> x :: r) p ((s *> m) <|> return [])) in
  obs_phrase_list
  <|> (sep (char ',') phrase)

let is_ftext = function
  | '\033' .. '\057'
  | '\059' .. '\126' -> true
  | _ -> false

let implode l =
  let s = Bytes.create (List.length l) in
  let rec aux i = function
    | [] -> s
    | x :: r -> Bytes.set s i x; aux (i + 1) r
  in
  aux 0 l

let received_token =
  (addr_spec >>| fun (local, domain) -> `Addr (local, (domain, [])))
  <|> (angle_addr >>| fun v -> `Addr v)
  <|> (domain >>| fun v -> `Domain v)
  <|> (Rfc822.word >>| fun v -> `Word v)

let received =
  many received_token
  >>= fun lst -> option None (char ';' *> date_time >>| fun v -> Some v)
  >>| fun rst -> (lst, rst)

let path =
  ((angle_addr >>| fun v -> Some v)
   <|> (option () Rfc822.cfws
        *> char '<'
        *> option () Rfc822.cfws
        *> char '>'
        *> option () Rfc822.cfws
        *> return None))
  <|> (addr_spec >>| fun (local, domain) -> Some (local, (domain, [])))

let field_name =
  one (satisfy is_ftext) >>| implode

let trace path =
  let r =
    string
      String.lowercase_ascii
      "Received"
    *> (many (satisfy (function '\x09' | '\x20' -> true | _ -> false)))
    *> char ':'
    *> received <* Rfc822.crlf
  in match path with
  (* we recognize Return-Path *)
  | Some path -> one r >>| fun traces -> (path, traces)
  (* we recognize Received *)
  | None -> received <* Rfc822.crlf >>= fun pre -> many r >>| fun rst -> (None, pre :: rst)

let field extend field_name =
  match String.lowercase_ascii field_name with
  | "date"              -> date_time <* Rfc822.crlf >>| fun v        -> `Date v
  | "from"              -> mailbox_list <* Rfc822.crlf >>| fun v     -> `From v
  | "sender"            -> mailbox <* Rfc822.crlf >>| fun v          -> `Sender v
  | "reply-to"          -> address_list <* Rfc822.crlf >>| fun v     -> `ReplyTo v
  | "to"                -> address_list <* Rfc822.crlf >>| fun v     -> `To v
  | "cc"                -> address_list <* Rfc822.crlf >>| fun v     -> `Cc v
  | "bcc"               -> address_list <* Rfc822.crlf >>| fun v     -> `Bcc v
  | "message-id"        -> Rfc822.msg_id <* Rfc822.crlf >>| fun v    -> `MessageID v
  | "in-reply-to"       -> phrase_or_msg_id <* Rfc822.crlf >>| fun v -> `InReplyTo v
  | "references"        -> phrase_or_msg_id <* Rfc822.crlf >>| fun v -> `References v
  | "subject"           -> unstructured <* Rfc822.crlf >>| fun v     -> `Subject v
  | "comments"          -> unstructured <* Rfc822.crlf >>| fun v     -> `Comments v
  | "keywords"          -> keywords <* Rfc822.crlf >>| fun v         -> `Keywords v
  | "resent-date"       -> date_time <* Rfc822.crlf >>| fun v        -> `ResentDate v
  | "resent-from"       -> mailbox_list <* Rfc822.crlf >>| fun v     -> `ResentFrom v
  | "resent-sender"     -> mailbox <* Rfc822.crlf >>| fun v          -> `ResentSender v
  | "resent-to"         -> address_list <* Rfc822.crlf >>| fun v     -> `ResentTo v
  | "resent-cc"         -> address_list <* Rfc822.crlf >>| fun v     -> `ResentCc v
  | "resent-bcc"        -> address_list <* Rfc822.crlf >>| fun v     -> `ResentBcc v
  | "resent-message-id" -> Rfc822.msg_id <* Rfc822.crlf >>| fun v    -> `ResentMessageID v
  | "resent-reply-to"   -> address_list <* Rfc822.crlf >>| fun v     -> `ResentReplyTo v
  | "received"          -> trace None >>| fun v                      -> `Trace v
  | "return-path"       -> path <* Rfc822.crlf
                           >>= fun v -> trace (Some v)
                           >>| fun v -> `Trace v
  | _             ->
    ((extend field_name)
     <|> (unstructured <* Rfc822.crlf >>| fun v -> `Field (field_name, v)))

let sp = Format.sprintf

let field extend field_name =
  (field extend field_name)
  <|> ((unstructured <* Rfc822.crlf >>| fun v -> `Unsafe (field_name, v)) <?> (sp "Unsafe %s" field_name))

type err += Nothing_to_do

let skip =
  let fix' f =
    let rec u a = lazy (f r a)
    and r a = { f = fun i s fail succ ->
              Lazy.(force (u a)).f i s fail succ }
    in r
  in

  { f = fun i s fail' succ ->
    let buffer = Buffer.create 16 in

    let consume =
      { f = fun i s fail succ ->
        let n = Input.transmit i @@ fun buff off len ->
          let len' = locate buff off len ((<>) '\r') in
          Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
          len'
        in

        succ i s n }
    in

    let succ' i s () =
      succ i s (Buffer.contents buffer) in

    let r = (fix' @@ fun m consumed -> consume >>= fun n -> peek_chr >>= fun chr ->
             match consumed + n, chr with
             | 0, _ -> fail Nothing_to_do
             | n, Some _ -> (Rfc822.crlf <|> m n)
             | n, None -> return ()) in

    (r 0).f i s fail' succ' }

let header extend =
  many ((field_name
         <* (many (satisfy (function '\x09' | '\x20' -> true | _ -> false)))
         <* char ':'
         >>= fun field_name -> field extend field_name)
        <|> (skip >>| fun v -> `Skip v))

let decode boundary rollback =
  { f = fun i s fail succ ->
    let buffer = Buffer.create 16 in

    let store buff off len =
      let len' = locate buff off len ((<>) '\r') in
      Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
      len'
    in

    (fix @@ fun m ->
     { f = fun i s fail succ ->
       let n = Input.transmit i store in
       succ i s n } *> peek_chr >>= function
     | Some '\r' ->
       (boundary *> return (true, Buffer.contents buffer))
       <|> (Rfc822.crlf >>= fun () -> Buffer.add_char buffer '\n';
                                      m)
       <|> (advance 1 >>= fun () -> Buffer.add_char buffer '\r';
                                    m)
     | Some chr -> m
     | None -> return (false, Buffer.contents buffer)).f i s fail succ }
  >>= function
     | true, content  -> rollback *> return content
     | false, content -> return content
