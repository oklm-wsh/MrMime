type month =
  [ `Jan | `Feb | `Mar | `Apr
  | `May | `Jun | `Jul | `Aug
  | `Sep | `Oct | `Nov | `Dec ]

type day =
  [ `Mon | `Tue | `Wed | `Thu
  | `Fri | `Sat | `Sun ]

type tz =
  [ `TZ of int
  | `UT
  | `GMT | `EST | `EDT | `CST | `CDT
  | `MST | `MDT | `PST | `PDT
  | `Military_zone of char ]

type date      = int * month * int
type time      = int * int * int option
type date_time = day option * date * time * tz

type atom    = [ `Atom of string ]
type word    = [ atom | `String of string ]
type phrase  = [ word | `Dot | `WSP ] list
type text    = [ word | `WSP | Rfc2047.encoded ] list

type domain =
  [ `Literal of string
  | `Domain of atom list ]

type local   = word list
type mailbox = local * domain list
type person  = phrase option * mailbox
type group   = phrase * person list
type address = [ `Group of group | `Person of person ]

type left   = local
type right  = domain
type msg_id = left * right

type received =
  [ `Domain of domain
  | `Mailbox of mailbox
  | `Word of word ]

type field =
  [ `From            of person list
  | `Date            of date_time
  | `Sender          of person
  | `ReplyTo         of address list
  | `To              of address list
  | `Cc              of address list
  | `Bcc             of address list
  | `Subject         of text
  | `Comments        of text
  | `Keywords        of phrase list
  | `MessageID       of msg_id
  | `InReplyTo       of [`Phrase of phrase | `MsgID of msg_id] list
  | `References      of [`Phrase of phrase | `MsgID of msg_id] list
  | `ResentDate      of date_time
  | `ResentFrom      of person list
  | `ResentSender    of person
  | `ResentTo        of address list
  | `ResentCc        of address list
  | `ResentBcc       of address list
  | `ResentMessageID of msg_id
  | `Received        of received list * date_time option
  | `ReturnPath      of mailbox option
  | `Field           of string * text ]

let cur_chr ?(avoid = []) state =
  while List.exists ((=) (Lexer.cur_chr state)) avoid
  do state.Lexer.pos <- state.Lexer.pos + 1 done;

  Lexer.cur_chr state

(* See RFC 5322 § 4.1:

   obs-NO-WS-CTL   = %d1-8 /            ; US-ASCII control
                     %d11 /             ;  characters that do not
                     %d12 /             ;  include the carriage
                     %d14-31 /          ;  return, line feed, and
                     %d127              ;  white space characters
*)
let is_obs_no_ws_ctl = function
  | '\001' .. '\008'
  | '\011'
  | '\012'
  | '\014' .. '\031'
  | '\127' -> true
  | chr    -> false

let is_d0 = (=) '\000'
let is_lf = (=) '\n'
let is_cr = (=) '\r'

(* See RFC 5234 § Appendix B.1:

   VCHAR           = %x21-7E              ; visible (printing) characters
*)
let is_vchar = function
  | '\x21' .. '\x7e' -> true
  | chr              -> false

let s_vchar =
  let make a b incr =
    let rec aux acc i =
      if i > b then List.rev acc
      else aux (incr i :: acc) (incr i)
    in
    aux [a] a
  in

  make 0x21 0x7e ((+) 1) |> List.map Char.chr

(* See RFC 5234 § Appendix B.1:

   SP              = %x20
   HTAB            = %x09                 ; horizontal tab
   WSP             = SP / HTAB            ; white space
*)
let is_wsp = function
  | '\x20' | '\x09' -> true
  | chr             -> false

let s_wsp  = ['\x20'; '\x09']

let of_escaped_character = function
  | 'a' -> '\x07'
  | 'b' -> '\b'
  | 't' -> '\t'
  | 'n' -> '\n'
  | 'v' -> '\x0b'
  | 'f' -> '\x0c'
  | 'r' -> '\r'
  | chr -> chr

(* See RFC 5322 § 3.2.1:

   quoted-pair     = ("\" (VCHAR / WSP)) / obs-qp)
   obs-qp          = "\" (%d0 / obs-NO-WS-CTL / LF / CR)"
*)
let p_quoted_pair p state =
  (Logs.debug @@ fun m -> m "state: p_quoted_pair");

  Lexer.p_chr '\\' state;

  let chr = cur_chr state in

  if is_d0 chr
  || is_obs_no_ws_ctl chr
  || is_lf chr
  || is_cr chr
  || is_vchar chr
  || is_wsp chr
  then ((Logs.debug @@ fun m -> m "state: p_quoted_pair [%S]" (String.make 1 chr));
        Lexer.junk_chr state; p (of_escaped_character chr) state)
  else raise (Lexer.Error (Lexer.err_unexpected chr state))

(* See RFC 5322 § 3.2.2:

   FWS             = ([*WSP CRLF] 1*WSP) / obs-FWS
                                          ; Folding white space
   obs-FWS         = 1*WSP *(CRLF 1*WSP)

   XXX: it's [FWS] (optionnal FWS), not FWS!, the bool argument of [p] inform
        if we are a FWS token and WSP token, or not.
        it's impossible to have [has_fws = true] and [has_wsp = false], a
        folding whitespace need at least one whitespace token.

        so for the case [has_fws = true] and [has_wsp = true], you can
        [assert false].
*)
let rec p_fws p state =
  (* verify if we have *WSP CRLF, if it's true,
     we return a [tmp] buffer containing *WSP CRLF data.

     it's not destructive, so you need to drop data with [tmp] if you try to get
     the input after the CRLF. *)
  let has_line state =
    let tmp = Buffer.create 16 in
    let rec loop cr_read j =
      Buffer.add_char tmp (Bytes.get state.Lexer.buffer j);

      if j >= state.Lexer.len then false
      else
        match Bytes.get state.Lexer.buffer j with
        | '\n' ->
          if cr_read then true else loop false (j + 1)
        | '\r' ->
          loop true (j + 1)
        | '\x20' | '\x09' -> loop false (j + 1)
        | _ -> false
    in
    if loop false state.Lexer.pos
    then Some (Buffer.contents tmp)
    else None
  in

  let trim s =
    let len = Bytes.length s in
    let i   = ref 0 in

    while !i < len && is_wsp (Bytes.get s !i)
    do incr i done;

    !i <> 0, Bytes.sub s !i (len - !i)
  in

  (* for *(CRLF 1*WSP) loop  *)
  let rec end_of_line has_wsp has_fws success fail state =
    match has_line state with
    | Some tmp ->
      (Logs.debug @@ fun m -> m "state: p_fws/tmp %S" tmp);

      state.Lexer.pos <- state.Lexer.pos + String.length tmp;

      Lexer.read_line
        (fun state ->
         (Logs.debug @@ fun m -> m "state: p_fws/read_line");

         match cur_chr state with
         (* … 1*(CRLF 1*WSP) *)
         | '\x20' | '\x09' ->
           (Logs.debug @@ fun m -> m "state: p_fws … 1*(CRLF 1*WSP)");

           (* drop 1*WSP *)
           let _ = Lexer.p_while is_wsp state in
           success true true state
         (* … *(CRLF 1*WSP) CRLF e *)
         | chr ->
           let has_wsp, tmp = trim tmp in
           Lexer.roll_back (p has_wsp has_fws) tmp state)
        state
    (* … e *)
    | None -> fail has_wsp has_fws state
  in

  let rec loop state =
    match cur_chr state with
    (* WSP / CR *)
    | '\x20' | '\x09' | '\r' as chr ->
      let success has_wsp has_fws state =
        (Logs.debug @@ fun m -> m "state: p_fws/loop/success");

        match chr with
        (* 1*WSP *(CRLF 1*WSP), so it's obs-fws *)
        | '\x20' | '\x09' ->
          (* it's 1*WSP 1*(CRLF 1*WSP) loop:
             we can have multiple CRLF 1*WSP because we are in obs-fws *)
          let rec loop _ _ state =
            end_of_line true true loop (fun _ _ -> p true true) state in

          loop true true state
        (* 1*(CRLF 1*WSP) *)
        | chr -> p true true state
      in
      let fail has_wsp has_fws state =
        (Logs.debug @@ fun m -> m "state: p_fws/loop/fail");

        (* WSP / CR *)
        match chr with
        | '\r' -> (* CR XXX: don't drop '\r', may be it's \r\n *)
          p has_wsp has_fws state
        | chr  -> (* we have 1*WSP, so we drop 1*WSP *)
          let _ = Lexer.p_while is_wsp state in p true has_fws state
      in

      (* state (WSP / CR) and try [*WSP CRLF] 1*WSP *)
      end_of_line false false success fail state
    (* no fws *)
    | chr -> p false false state
  in

  (Logs.debug @@ fun m -> m "state: p_fws");
  loop state

(* See RFC 5322 § 3.2.2:

   ctext           = %d33-39 /            ; Printable US-ASCII
                     %d42-91 /            ;  characters not including
                     %d93-126 /           ;  "(", ")", or "\\"
                     obs-ctext
   obs-ctext       = obs-NO-WS-CTL
*)
let is_ctext = function
  | '\033' .. '\039'
  | '\042' .. '\091'
  | '\093' .. '\126' -> true
  | chr -> is_obs_no_ws_ctl chr

let rec p_ctext = Lexer.p_while is_ctext

(* See RFC 5322 § 3.2.2:

   ccontent        = ctext / quoted-pair / comment
   comment         = "(" *([FWS] ccontent) [FWS] ")"
*)
let rec p_ccontent p state =
  (Logs.debug @@ fun m -> m "state: p_ccontent");

  match cur_chr state with
  | '\\' -> p_quoted_pair (fun chr -> p) state
  | '(' -> p_comment p state
  | chr -> let s = p_ctext state in
    (Logs.debug @@ fun m -> m "state: p_ccontent: %s" s);
    p state

and p_comment p state =
  (Logs.debug @@ fun m -> m "state: p_comment");

  Lexer.p_chr '(' state;

  let rec loop state =
    match cur_chr state with
    | ')' ->
      (Logs.debug @@ fun m -> m "state: p_comment/end");
      Lexer.junk_chr state;
      p state
    | chr ->
      (* XXX: we ignore if we passed a fws entity. *)
      p_fws (fun _ _ -> p_ccontent @@ p_fws @@ (fun _ _ -> loop)) state
  in

  loop state

(* See RFC 5322 § 3.2.2:

   CFWS            = (1*([FWS] comment) [FWS]) / FWS

   XXX: because we have only [FWS], it's [CFWS], not CFWS!
        so, we can't verify if we have really a FWS pattern, but fuck off!
*)
let p_cfws p state =
  (Logs.debug @@ fun m -> m "state: p_cfws");

  let rec loop has_fws has_comment state =
    (* [FWS] *)
    match cur_chr state with
    (* 1*([FWS] comment) *)
    | '(' ->
      (Logs.debug @@ fun m -> m "state: p_cfws 1*([FWS] comment)");

      p_comment (p_fws (fun has_wsp' has_fws' -> loop (has_fws || has_wsp' || has_fws') true)) state
      (* 1*([FWS] comment) [FWS], we ignore if we passed a fws entity. *)
    | chr ->
      (Logs.debug @@ fun m -> m "state: p_cfws/fws (has_fws: %b, has_comment: %b)" has_fws has_comment);

      match has_comment, has_fws with
      | true,  true
      | true,  false -> p true state (* comment) [FWS] *)
      | false, true  -> p true state (* / FWS *)
      | false, false -> p false state
      (* [FWS] e, we ignore if we passed a fws entity. *)
  in

  p_fws (fun has_wsp has_fws -> loop (has_wsp || has_fws) false) state

(* See RFC 5234 § 3.4:

   DIGIT           = %x30-39
*)
let is_digit = function
  | '\x30' .. '\x39' -> true
  | chr              -> false

(* See RFC 5234 § Appendix B.1:

   ALPHA           = %x41-5A / %x61-7A    ; A-Z / a-z
*)
let is_alpha = function
  | '\x41' .. '\x5a'
  | '\x61' .. '\x7A' -> true
  | chr              -> false

(* See RFC 5322 § 3.2.3:

   atext           = ALPHA / DIGIT /      ; Printable US-ASCII
                     "!" / "#" /          ;  characters not including
                     "$" / "%" /          ;  specials. Used for atoms.
                     "&" / "'" /
                     "*" / "+" /
                     "-" / "/" /
                     "=" / "?" /
                     "^" / "_" /
                     "`" / "{" /
                     "|" / "}" /
                     "~"
*)
let is_atext = function
  | '!' | '#'
  | '$' | '%'
  | '&' | '\''
  | '*' | '+'
  | '-' | '/'
  | '=' | '?'
  | '^' | '_'
  | '`' | '{'
  | '|' | '}'
  | '~' -> true
  | chr -> is_digit chr || is_alpha chr

let is_valid_atext text =
  let i = ref 0 in

  while !i < String.length text
        && is_atext (String.get text !i)
  do incr i done;

  if !i = String.length text
  then true
  else false

let p_atext state =
  (Logs.debug @@ fun m -> m "state: p_atext");
  let s = Lexer.p_while is_atext state in
  (Logs.debug @@ fun m -> m "state: p_atext %S" s);
  s

(* See RFC 5322 § 3.2.3:

   atom            = [CFWS] 1*atext [CFWS]
*)
let p_atom p =
  (Logs.debug @@ fun m -> m "state: p_atom");

  p_cfws
  @@ fun _ state -> let atext = p_atext state in p_cfws (fun _ -> p atext) state

(* See RFC 5322 § 3.2.3:

   dot-atom-text   = 1*atext *("." 1*atext)
*)
let p_dot_atom_text p state =
  (Logs.debug @@ fun m -> m "state: p_dot_atom_text");

  let rec next acc state =
    (Logs.debug @@ fun m -> m "state: p_dot_atom_text/next");

    match cur_chr state with
    | '.' -> Lexer.junk_chr state; next (`Atom (p_atext state) :: acc) state
    | chr -> p (List.rev acc) state
  in

  next [`Atom (p_atext state)] state

(* See RFC 5322 § 3.2.3:

   dot-atom        = [CFWS] dot-atom-text [CFWS]
*)
let p_dot_atom p =
  (Logs.debug @@ fun m -> m "state: p_dot_atom");

  p_cfws @@ (fun _ -> p_dot_atom_text (fun lst -> p_cfws (fun _ -> p lst)))

(* See RFC 5322 § 3.2.3:

   specials        = %x28 / %x29 /        ; Special characters that do
                     "<"  / ">"  /        ;  not appear in atext
                     "["  / "]"  /
                     ":"  / ";"  /
                     "@"  / %x5C /
                     ","  / "."  /
                     DQUOTE

   See RFC 5234 § Appendix B.1:

   DQUOTE          = %x22
                                          ; (Double Quote)
*)
let is_specials = function
  | '(' | ')'
  | '<' | '>'
  | '[' | ']'
  | ':' | ';'
  | '@' | '\\'
  | ',' | '.'
  | '"' -> true
  | chr -> false

(* See RFC 5322 § 3.2.4:

   qtext           = %d33 /               ; Printable US-ASCII
                     %d35-91 /            ;  characters not including
                     %d93-126 /           ;  %x5C or the quote character
                     obs-qtext

   obs-qtext       = obs-NO-WS-CTL
*)
let is_qtext = function
  | '\033'
  | '\035' .. '\091'
  | '\093' .. '\126' -> true
  | chr              -> is_obs_no_ws_ctl chr

let is_dquote = (=) '"'

let p_qtext state =
  (Logs.debug @@ fun m -> m "state: p_qtext");
  let s = Lexer.p_while is_qtext state in
  (Logs.debug @@ fun m -> m "state: p_qtext: %S" s);
  s

(* See RFC 5322 § 3.2.4:

   qcontent        = qtext / quoted-pair
*)
let p_qcontent p state =
  (Logs.debug @@ fun m -> m "state: p_qcontent");

  match cur_chr state with
  | '\\' -> p_quoted_pair (fun chr -> p (String.make 1 chr)) state
  | chr when is_qtext chr -> p (p_qtext state) state
  | chr -> raise (Lexer.Error (Lexer.err_unexpected chr state))

(* See RFC 5322 § 3.2.4:

   quoted-string   = [CFWS]
                     DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                     [CFWS]
*)
let p_quoted_string p state =
  (Logs.debug @@ fun m -> m "state: p_quoted_string");

  let rec loop acc state =
    match cur_chr state with
    | '"' ->
      Lexer.p_chr '"' state;
      p_cfws (fun _ -> p (List.rev acc |> String.concat "")) state
    | chr ->
      p_qcontent
        (fun str ->
         p_fws (fun has_wsp has_fws ->
                (Logs.debug @@ fun m -> m "has_wsp: %b" has_wsp);
                loop (if has_wsp then " " :: str :: acc else str :: acc)))
        state
  in
  p_cfws (fun _ state -> Lexer.p_chr '"' state; loop [] state) state

(* See RFC 5322 § 3.2.5:

   word            = atom / quoted-string
*)
let p_word p state =
  (Logs.debug @@ fun m -> m "state: p_word");

  let loop has_fws state =
    match cur_chr state with
    | '"' ->
      (Logs.debug @@ fun m -> m "state: p_word/loop to p_quoted_string [%S]" (String.make 1 '"'));
      p_quoted_string (fun s -> p (`String s)) state
    | chr ->
      p_atom (fun s -> p (`Atom s)) state
  in

  p_cfws (fun has_fws -> loop has_fws) state

(* See RFC 5322 § 3.2.5 & 4.1:

   phrase          = 1*word / obs-phrase
   obs-phrase      = word *(word / "." / CFWS)
*)
let p_phrase p state =
  (Logs.debug @@ fun m -> m "state: p_phrase");

  let add_fws has_fws element words =
    if has_fws
    then element :: `WSP :: words
    else element :: words
  in

  (* XXX: remove unused FWS, if we don't remove that, the pretty-printer raise
          an error. May be, we fix that in the pretty-printer but I decide to
          fix that in this place. *)
  let rec trim = function
    | `WSP :: r -> trim r
    | r -> r
  in

  let rec obs words state =
    (Logs.debug @@ fun m -> m "state: p_phrase/obs");

    p_cfws (fun has_fws state -> match cur_chr state with
            | '.' ->
              (Logs.debug @@ fun m -> m "state: p_phrase/obs (has_fws: %b)" has_fws);

              Lexer.junk_chr state; obs (add_fws has_fws `Dot words) state
            | chr when is_atext chr || is_dquote chr ->
              (Logs.debug @@ fun m -> m "state: p_phrase/obs (has_fws: %b)" has_fws);

              p_word (fun word -> obs (add_fws has_fws word words)) state
            | _ -> p (trim @@ List.rev @@ trim words) state)
      state
  in

  let rec loop words state =
    (Logs.debug @@ fun m -> m "state: p_phrase/loop");

    (* XXX: we catch [p_word] (with its [CFWS] in [p_atom]/[p_quoted_string])
            to determine if we need to switch to [obs] (if we have a '.'),
            or to continue [p_word] *)
    p_cfws (fun has_fws state -> match cur_chr state with
            | chr when is_atext chr || is_dquote chr ->
              (Logs.debug @@ fun m -> m "state: p_phrase/loop (has_fws: %b)" has_fws);

              p_word (fun word -> loop (add_fws true word words)) state
            (* XXX: may be it's '.', so we try to switch to obs *)
            | _ -> obs (if has_fws then `WSP :: words else words) state)
      state
  in

  p_word (fun word -> loop [word]) state

(* See RFC 5322 § 4.1:

   obs-utext       = %d0 / obs-NO-WS-CTL / VCHAR
*)
let is_obs_utext = function
  | '\000' -> true
  | chr -> is_obs_no_ws_ctl chr || is_vchar chr

(* See RFC 5322 § 4.1:

   obs-unstruct    = *(( *LF *CR *(obs-utext *LF *CR)) / FWS)

   XXX: old but not wrong (not tested) implementation without record data
*)
let p_obs_unstruct p state =
  (* 1*(obs-utext *LF *CR) *)
  let rec loop1 state =
    let _ = Lexer.p_while is_obs_utext state in

    if cur_chr state = '\n' then (ignore @@ Lexer.p_while is_lf state);
    if cur_chr state = '\r' then (ignore @@ Lexer.p_while is_cr state);

    if is_obs_utext (cur_chr state)
    then loop1 state
    else p state
  in

  (* *LF *CR *(obs-utext *LF *CR) *)
  let rec loop0 ?(c = `LF) state =
    match cur_chr state, c with
    | '\n', `LF -> let _ = Lexer.p_while is_lf state in loop0 ~c:`CR state
    | '\r', `LF
    | '\r', `CR -> let _ = Lexer.p_while is_cr state in loop0 ~c:`UT state
    | chr, (`LF | `CR | `UT) when is_obs_utext chr -> loop1 state
    | _ -> p state
  in

  (* *(( *LF *CR *(obs-utext *LF *CR )) / FWS)
     XXX: may be we use [has_fws] argument to re try this regexp or to go
          to [loop0] to try the other pattern than [FWS]. *)
  p_fws (fun _ _ state -> loop0 state) state

(* See RFC 5322 § 3.2.5

   unstructured    = ( *([FWS] VCHAR) *WSP) / obs-unstruct

   XXX: old but not wrong (not tested) implementation without record data
*)
let p_unstructured p state =
  let rec loop state =
    (* [FWS] *)
    p_fws (fun has_wsp has_fws state -> match has_wsp || has_fws, cur_chr state with
           (* *WSP, go to next *)
           | false, '\x20' | false, '\x09' ->
             let _ = Lexer.p_while is_wsp state in p state
           | true, chr when is_vchar chr ->
             Lexer.p_set s_vchar state; loop state
           | _, chr -> p_obs_unstruct p state)
           (* XXX: may be it's wrong to continue the lexing if [has_fws = true]
                   but, if I fail at this point, it's not good. *)
      state
  in

  loop state

(* See RFC 5322 § 4.1:

   obs-unstruct    = *(( *LF *CR *(obs-utext *LF *CR)) / FWS)

   XXX: we have an axiom in this rule, the next token should be a CRLF. And
        if we have a CRLF (after we try [FWS]), we let the compute to [p]. In
        another case, we continue to record the data.

        so, the rule is really: *.CRLF[^ WSP] (with '.' it's all character)
*)
let p_obs_unstruct p state =
  (Logs.debug @@ fun m -> m "state: p_obs_unstruct");

  let buf = Buffer.create 16 in
  let last buf =
    if Buffer.length buf > 0
    then fun c -> (Buffer.nth buf (Buffer.length buf - 1)) <> c
    else fun _ -> false
  in

  let rec loop1 state =
    (Logs.debug @@ fun m -> m "state: p_obs_unstruct/loop1");

    if is_obs_utext @@ cur_chr state
    then begin
      Buffer.add_char buf (cur_chr state);
      Lexer.junk_chr state;

      let lf = Lexer.p_repeat is_lf state in
      let cr = Lexer.p_repeat is_cr state in

      if String.length lf = 0
         && String.length cr = 1
         && is_lf @@ cur_chr state
      then Lexer.roll_back (p (Buffer.contents buf)) "\r" state
      else begin
        if last buf ' '
           && (String.length lf > 0 || String.length cr > 0)
        then Buffer.add_char buf ' ';

        loop1 state
      end
    end else p_fws loop0 state

  and loop0 has_wsp has_fws state =
    (Logs.debug @@ fun m -> m "state: p_obs_unstruct/loop0 (has_fws: %b)" has_fws);

    if has_wsp
    then begin Buffer.add_char buf ' '; p_fws loop0 state end
    else begin
      let lf = Lexer.p_repeat is_lf state in
      let cr = Lexer.p_repeat is_cr state in

      if String.length lf = 0
         && String.length cr = 1
         && is_lf @@ cur_chr state
      then Lexer.roll_back (p (Buffer.contents buf)) "\r" state
      else begin
        if last buf ' '
           && (String.length lf > 0 || String.length cr > 0)
        then Buffer.add_char buf ' ';

        loop1 state
      end
    end
  in

  p_fws loop0 state

(* See RFC 5322 § 3.2.5

   unstructured    = ( *([FWS] VCHAR) *WSP) / obs-unstruct

   XXX: same as [obs-unstruct]
*)
let p_unstructured p state =
  (Logs.debug @@ fun m -> m "state: p_unstructured");

  let buf = Buffer.create 16 in
  let last buf =
    if Buffer.length buf > 0
    then fun c -> (Buffer.nth buf (Buffer.length buf - 1)) <> c
    else fun _ -> false
  in

  let rec loop1 has_wsp has_fws state =
    (Logs.debug @@ fun m -> m "state: p_unstructured/loop1 (has_fws: %b)" has_fws);

    match cur_chr state with
    | chr when is_vchar chr ->
      if last buf ' ' && has_wsp
      then Buffer.add_char buf ' ';

      (Logs.debug @@ fun m -> m "state: p_unstructured/loop1 (chr: [%S])" (String.make 1 (cur_chr state)));
      Buffer.add_char buf (cur_chr state);
      Lexer.junk_chr state;
      p_fws loop1 state
    | _ ->
      p_obs_unstruct (fun data -> p (Buffer.contents buf ^ data)) state

  and loop0 state =
    (Logs.debug @@ fun m -> m "state: p_unstructured/loop0");

    p_fws (fun has_wsp has_fws state ->
         match cur_chr state, has_wsp with
         | ('\x20' | '\x09'), false ->
           let _ = Lexer.p_repeat is_wsp state in

           if last buf ' '
           then Buffer.add_char buf ' ';

           loop0 state
         | chr, has_wsp -> loop1 has_wsp has_fws state)
    state
  in

  loop0 state

(* XXX: bon là, j'écris en français parce que c'est vraiment de la merde. En
        gros le [obs-unstruct] ou le [unstructured], c'est de la grosse merde
        pour 3 points:

        * le premier, c'est que depuis la RFC 2047, on peut mettre DES
          [encoded-word] dans un [obs-unstruct] ou un [unstructured]. Il faut
          donc decoder ces fragments premièrement. MAIS il faut bien comprendre
          qu'un (ou plusieurs) espace entre 2 [encoded-word] n'a aucune
          signication - en gros avec: '=utf-8?Q?a=    =utf-8?Q?b=', on obtient
          'ab'. SAUF que dans le cas d'un [encoded-word 1*FWS 1*obs-utext]
          l'espace est significatif et ça moment là, tu te dis WTF! Bien
          entendu, les espaces entre deux [1*obs-utext] est tout autant
          signicatif. DONC OUI C'EST DE LA MERDE.

        * MAIS C'EST PAS FINI! Si on regarde bien la règle, cette pute, elle se
          termine pas. OUAIS OUAIS! En vrai, elle se termine après avoir essayer
          le token [FWS], après avoir essayer [*LF] et [*CR], qu'il y est au
          moins un des deux derniers token existant (donc soit 1*LF ou 1*CR) et
          qu'après avoir essayer à nouveau un [FWS] si on a pas de [obs-utext],
          on regarde si on a bien eu un token [FWS] (d'où la nécessité d'avoir
          [has_wsp] et [has_fws] dans la fonction [p_fws]). DONC (OUAIS C'EST LA
          MERDE), si on a bien un token [FWS], on recommence, SINON on termine.

        * ENFIN LE PIRE HEIN PARCE QUE ENCORE C'EST GENTIL! Comme on ESSAYE
          d'avoir un CR* à la fin, IL PEUT ARRIVER (j'ai bien dit il peut mais
          en vrai ça arrive tout le temps) qu'on consomme le CR du token CRLF
          OBLIGATOIRE à chaque ligne. DONC la fonction compile si tu termines
          par un CR ET SI C'EST LE CAS ON ROLLBACK pour récupérer le CR
          OBLIGATOIRE à la fin de ligne.

        DONC CETTE REGLE, C'EST CARREMENT DE LA MERDE ET VOILA POURQUOI CETTE
        FONCTION EST AUSSI COMPLEXE. Merci de votre attention.
*)
let p_obs_unstruct ?(acc = []) p state =
  let compile rlst state =
    let rec aux ?(previous = `None) acc l = match l, previous with
      | (`Encoded _ as enc) :: r, `LWSP ->
        aux ~previous:`Enc (enc :: `WSP :: acc) r
      | (`Encoded _ as enc) :: r, (`ELWSP | `None) ->
        aux ~previous:`Enc (enc :: acc) r
      | `Encoded _ :: r, (`Atom | `Enc)
      | `Atom _ :: r, `Enc ->
        assert false (* XXX: raise error *)
      | (`Atom _ as txt) :: r, (`LWSP | `ELWSP) ->
        aux ~previous:`Atom (txt :: `WSP :: acc) r
      | (`Atom s as txt) :: r, (`None | `Atom) ->
        aux ~previous:`Atom (txt :: acc) r
      | (`LF | `CR | `WSP | `FWS) :: r1 :: r2, (`ELWSP | `Enc) ->
        aux ~previous:`ELWSP acc (r1 :: r2)
      | (`LF | `CR | `WSP | `FWS) :: r1 :: r2, (`LWSP | `Atom) ->
        aux ~previous:`LWSP acc (r1 :: r2)
      | (`LF | `CR | `WSP | `FWS) :: r1 :: r2, `None ->
        aux ~previous:`None acc (r1 :: r2)
      | [ `CR ], _ ->
        Lexer.roll_back (fun state -> p (List.rev acc) state) "\r" state
      | [ (`LF | `WSP | `FWS) ], _ | [], _ ->
        p (List.rev acc) state
    in

    aux [] (List.rev rlst)
  in

  let rec data acc =
    Lexer.p_try_rule
      (fun (charset, encoding, s) state ->
       let lf = Lexer.p_repeat is_lf state in
       let cr = Lexer.p_repeat is_cr state in

       let acc' =
         match String.length lf, String.length cr with
         | 0, 0 -> `Encoded (charset, encoding, s) :: acc
         | n, 0 -> `Encoded (charset, encoding, s) :: `LF :: acc
         | 0, n -> `Encoded (charset, encoding, s) :: `CR :: acc
         | _    -> `Encoded (charset, encoding, s) :: `CR :: `LF :: acc
       in

       match cur_chr state with
       | chr when is_obs_utext chr -> data acc' state
       | chr -> loop acc' state)
      (fun state ->
       let ts = Lexer.p_while is_obs_utext state in
       let lf = Lexer.p_repeat is_lf state in
       let cr = Lexer.p_repeat is_cr state in

       let acc' =
         match String.length lf, String.length cr with
         | 0, 0 -> `Atom ts :: acc
         | n, 0 -> `Atom ts :: `LF :: acc
         | 0, n -> `Atom ts :: `CR :: acc
         | _    -> `Atom ts :: `CR :: `LF :: acc
       in

       match cur_chr state with
       | chr when is_obs_utext chr -> data acc' state
       | chr -> loop acc' state)
      (Rfc2047.p_encoded_word (fun charset encoding s state -> `Ok ((charset, encoding, s), state)))

  and lfcr acc state =
    let lf = Lexer.p_repeat is_lf state in
    let cr = Lexer.p_repeat is_cr state in

    let acc' = match String.length lf, String.length cr with
      | 0, 0 -> acc
      | n, 0 -> `LF :: acc
      | 0, n -> `CR :: acc
      | _    -> `CR :: `LF :: acc
    in

    match cur_chr state with
    | chr when is_obs_utext chr -> data acc state
    | chr when String.length lf > 0 || String.length cr > 0 ->
      p_fws (fun has_wsp has_fws ->
             match has_wsp, has_fws with
             | true, true   -> lfcr (`FWS :: acc')
             | true, false  -> lfcr (`WSP :: acc')
             | false, false -> compile acc'
             | _            -> assert false)
        state
    | _ -> compile acc' state

  and loop acc =
    p_fws (fun has_wsp has_fws ->
      match has_wsp, has_fws with
      | true, true   -> loop (`FWS :: acc)
      | true, false  -> loop (`WSP :: acc)
      | false, false -> lfcr acc
      | false, true  -> assert false)
  in

  loop acc state

let p_unstructured p state =
  let rec loop acc has_wsp has_fws state =
    match has_wsp, has_fws, Lexer.cur_chr state with
    | has_wsp, has_fws, chr when is_vchar chr ->
      let adder x =
        if has_fws && has_wsp
        then x :: `FWS :: acc
        else if has_wsp
        then x :: `WSP :: acc
        else x :: acc
      in
      Lexer.p_try_rule
        (fun (charset, encoding, s) ->
         p_fws (loop (adder (`Encoded (charset, encoding, s)))))
        (fun state ->
         let s = Lexer.p_while is_vchar state in
         p_fws (loop (adder (`Atom s))) state)
        (Rfc2047.p_encoded_word (fun charset encoding s state -> `Ok ((charset, encoding, s), state)))
        state
    | true, true, _   -> p_obs_unstruct ~acc:(`FWS :: acc) p state
    | true, false, _  -> p_obs_unstruct ~acc:(`WSP :: acc) p state
    | false, false, _ -> p_obs_unstruct ~acc p state
    | false, true, _  -> assert false
  in

  p_fws (loop []) state


(* [CFWS] 2DIGIT [CFWS] *)
let p_cfws_2digit_cfws p state =
  (Logs.debug @@ fun m -> m "state: p_cfws_2digit_cfws");

  p_cfws (fun _ state -> let n = Lexer.p_repeat ~a:2 ~b:2 is_digit state in
                       p_cfws (p (int_of_string n)) state) state
(* See RFC 5322 § 4.3:

   obs-hour        = [CFWS] 2DIGIT [CFWS]
   obs-minute      = [CFWS] 2DIGIT [CFWS]
   obs-second      = [CFWS] 2DIGIT [CFWS]
*)
let p_obs_hour p state =
  (Logs.debug @@ fun m -> m "state: p_obs_hour");
  p_cfws_2digit_cfws (fun n _ -> p n) state

let p_obs_minute p state =
  (Logs.debug @@ fun m -> m "state: p_obs_minute");
  p_cfws_2digit_cfws (fun n _ -> p n) state

let p_obs_second p state =
  (Logs.debug @@ fun m -> m "state: p_obs_second");
  p_cfws_2digit_cfws p state

(* See RFC 5322 § 3.3:

   hour            = 2DIGIT / obs-hour
   minute          = 2DIGIT / obs-minute
   second          = 2DIGIT / obs-second
*)
let p_2digit_or_obs p state =
  (Logs.debug @@ fun m -> m "state: p_2digit_or_obs");

  if Lexer.p_try is_digit state = 2
  then let n = Lexer.p_while is_digit state in
       p_cfws (p (int_of_string n)) state
       (* XXX: in this case, it's possible to
               be in [obs] version, so we try
               [CFWS] *)
  else p_cfws_2digit_cfws p state

let p_hour p state =
  (Logs.debug @@ fun m -> m "state: p_hour");
  p_2digit_or_obs (fun n _ -> p n) state

let p_minute p state =
  (Logs.debug @@ fun m -> m "state: p_minute");
  p_2digit_or_obs p state

let p_second p state =
  (Logs.debug @@ fun m -> m "state: p_second");
  p_2digit_or_obs p state

(* See RFC 5322 § 3.3 & 4.3:

   year            = (FWS 4*DIGIT FWS) / obs-year
   obs-year        = [CFWS] 2*DIGIT [CFWS]
*)
let p_obs_year p state =
  (* [CFWS] 2*DIGIT [CFWS] *)
  p_cfws (fun _ state -> let y = Lexer.p_repeat ~a:2 is_digit state in
                         p_cfws (fun _ -> p (int_of_string y)) state) state

let p_year has_already_fws p state =
  (Logs.debug @@ fun m -> m "state: p_year");

  (* (FWS 4*DIGIT FWS) / obs-year *)
  p_fws (fun has_wsp has_fws state ->
    if (has_wsp || has_fws || has_already_fws) && Lexer.p_try is_digit state >= 4
    then let y = Lexer.p_while is_digit state in
         p_fws (fun has_wsp has_fws state ->
                if has_wsp || has_fws
                then p (int_of_string y) state
                else raise (Lexer.Error (Lexer.err_expected ' ' state))) state
    else p_obs_year p state)
  state

(* See RFC 5322 § 3.3 & 4.3:

   day             = ([FWS] 1*2DIGIT FWS) / obs-day
   obs-day         = [CFWS] 1*2DIGIT [CFWS]
*)
let p_obs_day p state =
  (Logs.debug @@ fun m -> m "state: p_obs_day");

  p_cfws (fun _ state -> let d = Lexer.p_repeat ~a:1 ~b:2 is_digit state in
                         p_cfws (fun _ -> p (int_of_string d)) state)
    state

let p_day p state =
  (Logs.debug @@ fun m -> m "state: p_day");

  p_fws (fun _ _ state ->
         (Logs.debug @@ fun m -> m "state: p_day (chr: [%S])" (String.make 1 (cur_chr state)));

         if is_digit @@ cur_chr state
         then let d = Lexer.p_repeat ~a:1 ~b:2 is_digit state in
              p_fws (fun has_wsp has_fws ->
                     (Logs.debug @@ fun m -> m "state: p_day (has_fws: %b)" has_fws);

                     if has_wsp || has_fws
                     then p (int_of_string d)
                     else raise (Lexer.Error (Lexer.err_expected ' ' state))) state
         else p_obs_day p state)
    state

(* See RFC 5322 § 3.3:

   month           = "Jan" / "Feb" / "Mar" / "Apr" /
                     "May" / "Jun" / "Jul" / "Aug" /
                     "Sep" / "Oct" / "Nov" / "Dec"
*)
let p_month p state =
  (Logs.debug @@ fun m -> m "state: p_month");

  let month = Lexer.p_repeat ~a:3 ~b:3 is_alpha state in

  let month = match month with
  | "Jan" -> `Jan
  | "Feb" -> `Feb
  | "Mar" -> `Mar
  | "Apr" -> `Apr
  | "May" -> `May
  | "Jun" -> `Jun
  | "Jul" -> `Jul
  | "Aug" -> `Aug
  | "Sep" -> `Sep
  | "Oct" -> `Oct
  | "Nov" -> `Nov
  | "Dec" -> `Dec
  | str   -> raise (Lexer.Error (Lexer.err_unexpected_str str state)) in

  p month state

(* See RFC 5322 § 3.3:

   day-name        = "Mon" / "Tue" / "Wed" / "Thu" /
                     "Fri" / "Sat" / "Sun"
*)
let p_day_name p state =
  (Logs.debug @@ fun m -> m "state: p_day_name");

  let day = Lexer.p_repeat ~a:3 ~b:3 is_alpha state in

  let day = match day with
  | "Mon" -> `Mon
  | "Tue" -> `Tue
  | "Wed" -> `Wed
  | "Thu" -> `Thu
  | "Fri" -> `Fri
  | "Sat" -> `Sat
  | "Sun" -> `Sun
  | str   -> raise (Lexer.Error (Lexer.err_unexpected_str str state)) in

  p day state

(* See RFC 5322 § 3.3 & 4.3:

   day-of-week     = ([FWS] day-name) / obs-day-of-week
   obs-day-of-week = [CFWS] day-name [CFWS]
*)
let p_day_of_week p =
  (Logs.debug @@ fun m -> m "state: p_day_of_week");

  p_fws
  @@ fun _ _ state ->
     if is_alpha (cur_chr state) then p_day_name p state
     else p_cfws (fun _ -> p_day_name (fun day -> p_cfws (fun _ -> p day)))
            state

(* See RFC 5322 § 3.3;

   date            = day month year
*)
let p_date p =
  (Logs.debug @@ fun m -> m "state: p_date");

  p_day (fun d -> p_month (fun m -> p_year false (fun y -> p (d, m, y))))

(* See RFC 5322 § 3.3:

   time-of-day     = hour ":" minute [ ":" second ]
*)
let p_time_of_day p =
  p_hour
  @@ (fun hh state ->
      Lexer.p_chr ':' state;
      p_minute
      (fun mm has_fws state -> match cur_chr state with
       | ':' -> Lexer.p_chr ':' state;
                p_second (fun ss has_fws -> p has_fws (hh, mm, Some ss)) state
       | chr -> p has_fws (hh, mm, None) state)
      state)

(* See RFC 5322 § 3.3:

   obs-zone        = "UT" / "GMT" /     ; Universal Time
                                        ; North American UT
                                        ; offsets
                     "EST" / "EDT" /    ; Eastern:  - 5/ - 4
                     "CST" / "CDT" /    ; Central:  - 6/ - 5
                     "MST" / "MDT" /    ; Mountain: - 7/ - 6
                     "PST" / "PDT" /    ; Pacific:  - 8/ - 7
                     %d65-73 /          ; Military zones - "A"
                     %d75-90 /          ; through "I" and "K"
                     %d97-105 /         ; through "Z", both
                     %d107-122          ; upper and lower case
*)
let p_obs_zone p state =
  let k x = p x state in
  match cur_chr state with
  | '\065' .. '\073' ->
    let a = cur_chr state in
    Lexer.junk_chr state;

    if a = 'G' || a = 'E' || a = 'C'
       && (cur_chr state = 'M' || cur_chr state = 'S' || cur_chr state = 'D')
    then let next = Lexer.p_repeat ~a:2 ~b:2 is_alpha state in
         match a, next with
         | 'G', "MT" -> k `GMT
         | 'E', "ST" -> k `EST
         | 'E', "DT" -> k `EDT
         | 'C', "ST" -> k `CST
         | 'C', "DT" -> k `CDT
         | chr, str ->
           let str = String.make 1 chr ^ str in
           raise (Lexer.Error (Lexer.err_unexpected_str str state))
    else k (`Military_zone a)
  | '\075' .. '\090' ->
    let a = cur_chr state in
    Lexer.junk_chr state;

    if a = 'U' && (cur_chr state = 'T')
    then (Lexer.p_chr 'T' state; k `UT)
    else if a = 'M' || a = 'P'
            && (cur_chr state = 'S' || cur_chr state = 'D')
    then let next = Lexer.p_repeat ~a:2 ~b:2 is_alpha state in
         match a, next with
         | 'M', "ST" -> k `MST (* maladie sexuellement transmissible *)
         | 'M', "DT" -> k `MDT
         | 'P', "ST" -> k `PST
         | 'P', "DT" -> k `PDT
         | chr, str ->
           let str = String.make 1 chr ^ str in
           raise (Lexer.Error (Lexer.err_unexpected_str str state))
    else k (`Military_zone a)
  | '\097' .. '\105' as a -> Lexer.junk_chr state; k (`Military_zone a)
  | '\107' .. '\122' as a -> Lexer.junk_chr state; k (`Military_zone a)
  | chr -> raise (Lexer.Error (Lexer.err_unexpected chr state))

(* See RFC 5322 § 3.3:

   zone            = (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
*)
let p_zone has_already_fws p state =
  (Logs.debug @@ fun m -> m "state: p_zone %b" has_already_fws);

  p_fws (fun has_wsp has_fws state ->
         match has_already_fws || has_wsp || has_fws, cur_chr state with
         | true, '+' ->
           Lexer.p_chr '+' state;
           let tz = Lexer.p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (int_of_string tz)) state
         | true, '-' ->
           Lexer.p_chr '-' state;
           let tz = Lexer.p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (- (int_of_string tz))) state
         | true, chr when is_digit chr ->
           let tz = Lexer.p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (int_of_string tz)) state
         | _ -> p_obs_zone p state)
    state

(* See RFC 5322 § 3.3:

   time            = time-of-day zone
*)
let p_time p state =
  (Logs.debug @@ fun m -> m "state: p_time");

  p_time_of_day
    (fun has_fws (hh, mm, dd) -> p_zone has_fws (fun tz -> p ((hh, mm, dd), tz)))
    state

(* See RFC 5322 § 3.3:

   date-time       = [ day-of-week "," ] date time [CFWS]
*)
let p_date_time p state =
  (Logs.debug @@ fun m -> m "state: p_date_time");

  let aux ?day state =
    (Logs.debug @@ fun m -> m "state: p_date_time/aux");

    p_date
      (fun (d, m, y) ->
       p_time (fun ((hh, mm, ss), tz) ->
               p_cfws (fun _ ->
                       (Logs.debug @@ fun m -> m "state: p_date_time/end");
                       p (day, (d, m, y), (hh, mm, ss), tz))))
    state
  in

  p_fws (fun _ _ state ->
         if is_alpha @@ cur_chr state
         then p_day_of_week (fun day state -> Lexer.p_chr ',' state; aux ~day state) state
         else aux state)
    state

(* See RFC 5322 § 3.4.1 & 4.4:

   dtext           = %d33-90 /            ; Printable US-ASCII
                     %d94-126 /           ;  characters not including
                     obs-dtext            ;  "[", "]", or %x5C
   obs-dtext       = obs-NO-WS-CTL / quoted-pair
*)
let is_dtext = function
  | '\033' .. '\090'
  | '\094' .. '\126' -> true
  | chr -> is_obs_no_ws_ctl chr

let p_dtext p state =
  let rec loop acc state =
    match cur_chr state with
    | '\033' .. '\090'
    | '\094' .. '\126' ->
      let s = Lexer.p_while is_dtext state in
      loop (s :: acc) state
    | chr when is_obs_no_ws_ctl chr ->
      let s = Lexer.p_while is_dtext state in
      loop (s :: acc) state
    | '\\' ->
      p_quoted_pair
        (fun chr state -> loop (String.make 1 chr :: acc) state) state
    | chr -> p (List.rev acc |> String.concat "") state
  in

  loop [] state

(* See RFC 5322 § 4.4:

   obs-domain      = atom *("." atom)
*)
let p_obs_domain p =
  let rec loop acc state =
    match cur_chr state with
    | '.' -> Lexer.junk_chr state; p_atom (fun o -> loop (`Atom o :: acc)) state
    | chr -> p (List.rev acc) state
  in

  p_atom (fun first -> loop [`Atom first])

(* See RFC 5322 § 4.4:

   obs-local-part  = word *("." word)
*)
let p_obs_local_part p =
  let rec loop acc state =
    match cur_chr state with
    | '.' -> Lexer.junk_chr state; p_word (fun o -> loop (o :: acc)) state
    | chr -> p (List.rev acc) state
  in

  p_word (fun first -> loop [first])

(* See RFC 5322 § 4.4:

   obs-group-list  = 1*([CFWS] ",") [CFWS]
*)
let p_obs_group_list p state =
  let rec loop state =
    match cur_chr state with
    | ',' -> Lexer.junk_chr state; p_cfws (fun _ -> loop) state
    | chr -> p_cfws (fun _ -> p) state
  in

  p_cfws (fun _ state -> match cur_chr state with
          | ',' -> Lexer.junk_chr state; p_cfws (fun _ -> loop) state
          | chr -> raise (Lexer.Error (Lexer.err_expected ',' state)))
    state

(* See RFC 5322 § 3.4.1:

  domain-literal   = [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
*)
let p_domain_literal p =
  let rec loop acc state =
    match cur_chr state with
    | ']' ->
      Lexer.p_chr ']' state;
      p_cfws (fun _ -> p (List.rev acc |> String.concat "")) state
    | chr when is_dtext chr || chr = '\\' ->
      p_dtext (fun s -> p_fws (fun _ _ -> loop (s :: acc))) state
    | chr -> raise (Lexer.Error (Lexer.err_unexpected chr state))
  in

  p_cfws (fun _ state ->
          match cur_chr state with
          | '[' -> Lexer.p_chr '[' state; p_fws (fun _ _ -> loop []) state
          | chr -> raise (Lexer.Error (Lexer.err_expected '[' state)))

(* See RFC 5322 § 3.4.1:

   domain          = dot-atom / domain-literal / obs-domain
*)
let p_domain p =
  let p_obs_domain' p =
    let rec loop acc state =
      match cur_chr state with
      | '.' -> Lexer.junk_chr state; p_atom (fun o -> loop (`Atom o :: acc)) state
      | chr -> p (List.rev acc) state
    in

    p_cfws (fun _ -> loop [])
  in

  (* XXX: dot-atom, domain-literal or obs-domain start with [CFWS] *)
  p_cfws (fun _ state ->
    match cur_chr state with
    (* it's domain-literal *)
    | '[' -> p_domain_literal (fun s -> p (`Literal s)) state
    (* it's dot-atom or obs-domain *)
    | chr ->
      p_dot_atom   (* may be we are [CFWS] allowed by obs-domain *)
        (function
         (* if we have an empty list, we need at least one atom *)
         | [] -> p_obs_domain (fun domain -> p (`Domain domain))
         (* in other case, we have at least one atom *)
         | l1 -> p_obs_domain' (fun l2 -> p (`Domain (l1 @ l2)))) state)

(* See RFC 5322 § 3.4.1:

   local-part      = dot-atom / quoted-string / obs-local-part

   XXX: same as domain
*)
let p_local_part p =
  (Logs.debug @@ fun m -> m "state: p_local_part");

  let p_obs_local_part' acc =
    (Logs.debug @@ fun m -> m "state: p_local_part/obs");

    let rec loop acc state =
      match cur_chr state with
      | '.' -> Lexer.junk_chr state; p_word (fun o -> loop (o :: acc)) state
      | chr -> p (List.rev acc) state
    in

    p_cfws (fun _ -> loop acc)
  in

  p_cfws (fun _ state ->
    match cur_chr state with
    | '"' -> p_quoted_string (fun s -> p_obs_local_part' [`String s]) state
             (* XXX: may be we should continue because it's [word] from
                     [obs-local-part] and it's not just [quoted-string]. *)
    | chr ->
      (* dot-atom / obs-local-part *)
      Lexer.p_try_rule
        (function
          | [] -> p_obs_local_part p
          | l  -> p_obs_local_part' (List.rev l))
        (p_obs_local_part p)
        (p_dot_atom (fun l state -> `Ok (l, state)))
        state)

(* See RFC 5322 § 3.4.1:

   addr-spec       = local-part "@" domain
*)
let p_addr_spec p state =
  (Logs.debug @@ fun m -> m "state: p_addr_spec");

  p_local_part (fun local_part state ->
                Lexer.p_chr '@' state;
                p_domain (fun domain -> p (local_part, domain)) state)
    state

(* See RFC 5322 § 4.4:

   obs-domain-list = *(CFWS / ",") "@" domain
                     *("," [CFWS] ["@" domain])
*)
let p_obs_domain_list p state =
  (* *("," [CFWS] ["@" domain]) *)
  let rec loop1 acc state =
    match cur_chr state with
    | ',' ->
      p_cfws
        (fun _ state -> match cur_chr state with
         | '@' ->
           Lexer.junk_chr state;
           p_domain (fun domain -> loop1 (domain :: acc)) state
         | chr -> p (List.rev acc) state)
        state
    | chr -> p (List.rev acc) state
  in

  (* *(CFWS / ",") "@" domain *)
  let rec loop0 state =
    match cur_chr state with
    | ',' -> Lexer.junk_chr state; loop0 state
    | '@' -> Lexer.junk_chr state; p_domain (fun domain -> loop1 [domain]) state
    (* XXX: may be raise an error *)
    | chr -> p_cfws (fun has_fws -> loop0) state
  in

  p_cfws (fun _ -> loop0) state

let p_obs_route p =
  p_obs_domain_list (fun domains state -> Lexer.p_chr ':' state; p domains state)

(* See RFC 5322 § 4.4:

   obs-angle-addr  = [CFWS] "<" obs-route addr-spec ">" [CFWS]
*)
let p_obs_angle_addr p state =
  (Logs.debug @@ fun m -> m "state: p_obs_angle_addr");

  p_cfws                                                 (* [CFWS] *)
    (fun _ state ->
      Lexer.p_chr '<' state;                             (* "<" *)
      p_obs_route                                        (* obs-route *)
        (fun domains ->
          p_addr_spec                                    (* addr-spec *)
            (fun (local_part, domain) state ->
              Lexer.p_chr '>' state;                     (* ">" *)
              p_cfws                                     (* [CFWS] *)
                (fun _ ->
                  p (local_part, domain :: domains)) state))
        state)
    state

(* See RFC 5322 § 3.4:

   angle-addr      = [CFWS] "<" addr-spec ">" [CFWS] /
                     obs-angle-addr
   ---------------------------------------------------
   obs-route       = obs-domain-list ":"
                   = *(CFWS / ",") "@" domain
                     *("," [CFWS] ["@" domain]) ":"
   ---------------------------------------------------
   angle-addr      = [CFWS] "<"
                     ├ *(CFWS / ",") "@" domain
                     │ *("," [CFWS] ["@" domain]) ":"
                     └ local-part "@" domain

                   = [CFWS] "<"
                     ├ *(CFWS / ",") "@" domain
                     │ *("," [CFWS] ["@" domain]) ":"
                     └ (dot-atom / quoted-string /
                        obs-local-part) "@" domain

                   = [CFWS] "<"
                     ├ *(CFWS / ",") "@" domain
                     │ *("," [CFWS] ["@" domain]) ":"
                     └ ('"' / atext) … "@" domain
   --------------------------------------------------
   [CFWS] "<"
   ├ if "," / "@" ─── *(CFWS / ",") ┐
   └ if '"' / atext ─ local-part    ┤
                                    │
   ┌──────────────────── "@" domain ┘
   ├ if we start with local-part    → ">" [CFWS]
   └ if we start with *(CFWS / ",") → *("," [CFWS] ["@" domain]) ":"
                                      addr-spec ">" [CFWS]
   --------------------------------------------------
   And, we have [p_try_rule] to try [addr-spec] firstly and 
   [obs-angle-addr] secondly.

   So, FUCK OFF EMAIL!
*)

let p_angle_addr p state =
  (Logs.debug @@ fun m -> m "state: p_angle_addr");

  let first p state =
    p_cfws
    (fun _ state ->
       Lexer.p_chr '<' state;
       p_addr_spec
       (fun (local_part, domain) state ->
          Lexer.p_chr '>' state;
          p_cfws (fun _ -> p (local_part, [domain])) state)
       state)
    state
  in

  Lexer.p_try_rule p (p_obs_angle_addr p)
    (first (fun data state -> `Ok (data, state)))
    state

(* See RFC 5322 § 3.4:

   display-name    = phrase
*)
let p_display_name p state =
  (Logs.debug @@ fun m -> m "state: p_display_name");
  p_phrase p state

(* See RFC 5322 § 3.4:

   name-addr       = [display-name] angle-addr
*)
let p_name_addr p state =
  (Logs.debug @@ fun m -> m "state: p_name_addr");

  p_cfws (fun _ state -> match cur_chr state with
    | '<' -> p_angle_addr (fun addr -> p (None, addr)) state
    | chr ->
      (Logs.debug @@ fun m -> m "state: p_name_addr (current chr %S)" (String.make 1 chr));

      p_display_name
        (fun name -> p_angle_addr (fun addr -> p (Some name, addr)))
        state)
    state

(* See RFC 5322 § 3.4:

   mailbox         = name-addr / addr-spec
*)
let p_mailbox p state =
  (Logs.debug @@ fun m -> m "state: p_mailbox");

  Lexer.p_try_rule p
    (p_addr_spec (fun (local_part, domain) -> p (None, (local_part, [domain]))))
    (p_name_addr (fun name_addr state -> `Ok (name_addr, state)))
    state

(* See RFC 5322 § 4.4:

   obs-mbox-list   = *([CFWS] ",") mailbox *("," [mailbox / CFWS])
*)
let p_obs_mbox_list p state =
  (Logs.debug @@ fun m -> m "state: p_obs_mbox_list");

  (* *("," [mailbox / CFWS]) *)
  let rec loop1 acc state =
    match cur_chr state with
    | ',' ->
      Lexer.junk_chr state;

      Lexer.p_try_rule
        (fun mailbox state -> loop1 (mailbox :: acc) state)
        (fun state -> p_cfws (fun _ -> loop1 acc) state)
        (fun state -> p_mailbox (fun data state -> `Ok (data, state)) state)
        state
    | chr -> p (List.rev acc) state
  in

  (* *([CFWS] ",") *)
  let rec loop0 state =
    match cur_chr state with
    | ',' -> Lexer.junk_chr state; p_cfws (fun _ -> loop0) state
    | chr -> p_mailbox (fun mailbox -> loop1 [mailbox]) state (* mailbox *)
  in

  p_cfws (fun _ -> loop0) state

(* See RFC 5322 § 3.4:

   mailbox-list    = (mailbox *("," mailbox)) / obs-mbox-list
*)
let p_mailbox_list p state =
  (Logs.debug @@ fun m -> m "state: p_mailbox_list");

  (* *("," [mailbox / CFWS]) *)
  let rec obs acc state =
    match cur_chr state with
    | ',' ->
      Lexer.junk_chr state;

      Lexer.p_try_rule
        (fun mailbox -> obs (mailbox :: acc))
        (p_cfws (fun _ -> obs acc))
        (p_mailbox (fun data state -> `Ok (data, state)))
        state
    | chr -> p (List.rev acc) state
  in

  (* *("," mailbox) *)
  let rec loop acc state =
    match cur_chr state with
    | ',' -> Lexer.junk_chr state; p_mailbox (fun mailbox -> loop (mailbox :: acc)) state
    | chr -> p_cfws (fun _ -> obs acc) state
  in

  p_cfws (fun _ state -> match cur_chr state with
          | ',' -> p_obs_mbox_list p state (* obs-mbox-list *)
          | chr ->
            p_mailbox
              (fun mailbox state -> match cur_chr state with
               | ',' -> loop [mailbox] state
               | chr -> p_cfws (fun _ -> obs [mailbox]) state)
              state)
    state

(* See RFC 5322 § 3.4:

   group-list      = mailbox-list / CFWS / obs-group-list
*)
let p_group_list p state =
  Lexer.p_try_rule
    (fun data -> p data)
    (Lexer.p_try_rule
       (fun () -> p [])
       (p_cfws (fun _ -> p []))
       (p_obs_group_list (fun state -> `Ok ((), state))))
    (p_mailbox_list (fun data state -> `Ok (data, state)))
    state

(* See RFC 5322 § 3.4:

   group           = display-name ":" [group-list] ";" [CFWS]
*)
let p_group p state =
  (Logs.debug @@ fun m -> m "state: p_group");

  p_display_name
    (fun display_name state ->
      Lexer.p_chr ':' state;

      (Logs.debug @@ fun m -> m "state: p_group (consume display name)");

      match cur_chr state with
      | ';' -> Lexer.p_chr ';' state; p_cfws (fun _ -> p (display_name, [])) state
      | chr ->
        p_group_list (fun group ->
          p_cfws (fun _ state ->
            Lexer.p_chr ';' state;
            p (display_name, group) state))
        state)
    state

(* See RFC 5322 § 3.4:

   address         = mailbox / group
*)
let p_address p state =
  (Logs.debug @@ fun m -> m "state: p_address");

  Lexer.p_try_rule
    (fun group state -> p (`Group group) state)
    (p_mailbox (fun mailbox -> p (`Person mailbox)))
    (p_group (fun data state -> `Ok (data, state)))
    state

(* See RFC 5322 § 4.4:

   obs-addr-list   = *([CFWS] ",") address *("," [address / CFWS])
*)
let p_obs_addr_list p state =
  (Logs.debug @@ fun m -> m "state: p_obs_addr");

  (* *("," [address / CFWS]) *)
  let rec loop1 acc state =
    (Logs.debug @@ fun m -> m "state: p_obs_addr/loop1");

    match cur_chr state with
    | ',' ->
      Lexer.junk_chr state;

      Lexer.p_try_rule
        (fun address -> loop1 (address :: acc))
        (p_cfws (fun _ -> loop1 acc))
        (p_address (fun data state -> `Ok (data, state)))
        state
    | chr -> p (List.rev acc) state
  in

  (* *([CFWS] ",") *)
  let rec loop0 state =
    (Logs.debug @@ fun m -> m "state: p_obs_addr/loop0");

    match cur_chr state with
    | ',' -> Lexer.junk_chr state; p_address (fun adress -> loop0) state
    | chr -> p_address (fun address -> loop1 [address]) state (* address *)
  in

  p_cfws (fun _ -> loop0) state

(* See RFC 5322 § 3.4:

   address-list    = (address *("," address)) / obs-addr-list
*)
let p_address_list p state =
  (Logs.debug @@ fun m -> m "state: p_address_list");

  (* *("," [address / CFWS]) *)
  let rec obs acc state =
    (Logs.debug @@ fun m -> m "state: p_address_list/obs");

    match cur_chr state with
    | ',' ->
      Lexer.junk_chr state;

      Lexer.p_try_rule
        (fun address -> obs (address :: acc))
        (p_cfws (fun _ -> obs acc))
        (p_address (fun data state -> `Ok (data, state)))
        state
    | chr -> p (List.rev acc) state
  in

  (* *("," address) *)
  let rec loop acc state =
    (Logs.debug @@ fun m -> m "state: p_address_list/loop");

    match cur_chr state with
    | ',' -> Lexer.junk_chr state;
      Lexer.p_try_rule
        (fun address -> loop (address :: acc))
        (p_cfws (fun _ -> obs acc))
        (p_address (fun address state -> `Ok (address, state)))
        state
      (* p_address (fun address -> loop (address :: acc)) state *)
    | chr -> p_cfws (fun _ -> obs acc) state
  in

  p_cfws (fun _ state -> match cur_chr state with
          | ',' -> p_obs_addr_list p state (* obs-addr-list *)
          | chr ->
            p_address
              (fun address state -> match cur_chr state with
               | ',' -> loop [address] state
               | chr -> p_cfws (fun _ -> obs [address]) state)
              state)
    state

(* See RFC 5322 § 3.6.4 & 4.5.4:

   id-left         = dot-atom-text / obs-id-left
   obs-id-left     = local-part
*)
let p_obs_id_left = p_local_part

let p_id_left p state =
  Lexer.p_try_rule p
    (p_obs_id_left p)
    (p_dot_atom_text (fun data state -> `Ok (data, state)))
    state

    (* See RFC 5322 § 3.6.4 & 4.5.4:

   id-right        = dot-atom-text / no-fold-literal / obs-id-right
   no-fold-literal = "[" *dtext "]"
   obs-id-right    =   domain
*)
let p_obs_id_right = p_domain

let p_no_fold_literal p state =
  Lexer.p_chr '[' state;
  p_dtext (fun d state -> Lexer.p_chr ']' state; p (`Literal d) state) state

let p_id_right p state =
  Lexer.p_try_rule p
    (Lexer.p_try_rule p
       (p_obs_id_right p)
       (p_no_fold_literal (fun data state -> `Ok (data, state))))
    (p_dot_atom_text (fun data state -> `Ok (`Domain data, state)))
    state

(* See RFC 5322 § 3.6.4:

   msg-id          = [CFWS] "<" id-left "@" id-right ">" [CFWS]
*)
let p_msg_id p state =
  p_cfws (fun _ state ->
    Lexer.p_chr '<' state;
    p_id_left (fun left state ->
      Lexer.p_chr '@' state;
      p_id_right (fun right state ->
        Lexer.p_chr '>' state;
        p_cfws (fun _ -> p (left, right)) state)
      state)
    state)
  state

let p_crlf p state =
  (Logs.debug @@ fun m -> m "state: p_crlf");

  Lexer.p_chr '\r' state;
  Lexer.p_chr '\n' state;
  p state

(* See RFC 5322 § 3.6.8:

   ftext           = %d33-57 /          ; Printable US-ASCII
                     %d59-126           ;  characters not including
                                          ;  ":".
*)
let is_ftext = function
  | '\033' .. '\057'
  | '\059' .. '\126' -> true
  | chr -> false

(* See RFC 5322 § 3.6.8:

   field-name      = 1*ftext
*)
let p_field_name = Lexer.p_repeat ~a:1 is_ftext

(* See RFC 5322 § 4.5.3:

   obs-bcc         = "Bcc" *WSP ":"
                     (address-list / ( *([CFWS] ",") [CFWS])) CRLF
*)
let p_obs_bcc p state =
  (Logs.debug @@ fun m -> m "state: p_obs_bcc");


  let rec aux state =
    p_cfws (fun _ state ->
      (Logs.debug @@ fun m -> m "state: p_obs_bcc/aux [%S]"
       (Bytes.sub state.Lexer.buffer state.Lexer.pos (state.Lexer.len -
       state.Lexer.pos)));

      match cur_chr state with
      | ',' -> aux state
      | chr -> p [] state)
      state
  in

  Lexer.p_try_rule p aux
    (p_address_list (fun l state -> `Ok (l, state))) state

(* See RFC 5322 § 3.6.3:

   bcc             = "Bcc:" [address-list / CFWS] CRLF
*)
let p_bcc p state =
  (Logs.debug @@ fun m -> m "state: p_bcc");

  Lexer.p_try_rule p
    (p_obs_bcc p)
    (p_address_list (fun l state -> `Ok (l, state))) state

(* phrase / msg-id for:

   references      = "References:" 1*msg-id CRLF
   obs-references  = "References" *WSP ":" *(phrase / msg-id) CRLF
   in-reply-to     = "In-Reply-To:" 1*msg-id CRLF
   obs-in-reply-to = "In-Reply-To" *WSP ":" *(phrase / msg-id) CRLF
*)
let p_phrase_or_msg_id p state =
  let rec loop acc =
    Lexer.p_try_rule
      (fun x -> loop (`MsgID x :: acc))
      (Lexer.p_try_rule
        (fun x -> loop (`Phrase x :: acc))
        (p (List.rev acc))
        (p_phrase (fun data state -> `Ok (data, state))))
      (p_msg_id (fun data state -> `Ok (data, state)))
  in

  loop [] state

(* See RFC 5322 § 3.6.7:

   received-token  = word / angle-addr / addr-spec / domain
*)
let p_received_token p state =
  let rec loop acc =
    Lexer.p_try_rule
      (fun data -> loop (`Domain data :: acc))
      (Lexer.p_try_rule
        (fun data -> loop (`Mailbox data :: acc))
        (Lexer.p_try_rule
          (fun data -> loop (`Mailbox data :: acc))
          (Lexer.p_try_rule
            (fun data -> loop (`Word data :: acc))
            (p (List.rev acc))
            (p_word (fun data state -> `Ok (data, state))))
          (p_addr_spec (fun (local, domain) state -> `Ok ((local, [domain]), state))))
        (p_angle_addr (fun data state -> `Ok (data, state))))
      (p_domain (fun data state -> `Ok (data, state)))
  in

  loop [] state

(* See RFC 5322 § 3.6.7:

   received        = "Received:" *received-token ";" date-time CRLF
   obs-received    = "Received" *WSP ":" *received-token CRLF
*)
let p_received p state =
  p_received_token
    (fun l state -> match cur_chr state with
     | ';' ->
       Lexer.p_chr ';' state;
       p_date_time (fun date_time -> p (l, Some date_time)) state
     | chr -> p (l, None) state)
    state

(* See RFC 5322 § 3.6.7:

   path            = angle-addr / ([CFWS] "<" [CFWS] ">" [CFWS])
*)
let p_path p =
  Lexer.p_try_rule
    (fun addr -> p (Some addr))
    (fun state ->
      p_cfws
        (fun _ state ->
          Lexer.p_chr '<' state;
          p_cfws
            (fun _ state ->
              Lexer.p_chr '>' state;
              p_cfws (fun _ -> p None) state)
            state)
        state)
    (p_angle_addr (fun data state -> `Ok (data, state)))

(* See RFC 5322 § 4.1:

   obs-phrase-list = [phrase / CFWS] *("," [phrase / CFWS])
*)
let p_obs_phrase_list p state =
  let rec loop acc state =
    Lexer.p_try_rule
      (fun s -> loop (s :: acc))
      (p_cfws (fun _ state ->
       match cur_chr state with
       | ',' -> Lexer.p_chr ',' state; loop acc state
       | chr -> p (List.rev acc) state))
      (fun state ->
       Lexer.p_chr ',' state;
       p_phrase (fun s state -> `Ok (s, state)) state)
      state
  in

  p_cfws (fun _ -> p_phrase (fun s -> loop [s])) state

(* See RFC 5322 § 3.6.5:

   keywords        = "Keywords:" phrase *("," phrase) CRLF
   obs-keywords    = "Keywords" *WSP ":" obs-phrase-list CRLF
*)
let p_keywords p state =
  let rec loop p acc =
    p_phrase (fun s state ->
      match cur_chr state with
      | ',' -> Lexer.p_chr ',' state; loop p (s :: acc) state
      | chr -> p_obs_phrase_list (fun l -> p (List.rev acc @ l)) state)
  in

  Lexer.p_try_rule
    (fun l -> p l)
    (p_obs_phrase_list p)
    (p_phrase (fun s -> loop (fun s state -> `Ok (s, state)) [s]))
    state

(* See RFC 5322 § 3.6.8:

   optional-field  = field-name ":" unstructured CRLF
   obs-optional    = field-name *WSP ":" unstructured CRLF
*)
let p_field p state =
  let field = p_field_name state in
  let _     = Lexer.p_repeat is_wsp state in

  Lexer.p_chr ':' state;

  let rule = match String.lowercase field with
    | "from"              -> p_mailbox_list     (fun l -> p_crlf @@ p (`From l))
    | "sender"            -> p_mailbox          (fun m -> p_crlf @@ p (`Sender m))
    | "reply-to"          -> p_address_list     (fun l -> p_crlf @@ p (`ReplyTo l))
    | "to"                -> p_address_list     (fun l -> p_crlf @@ p (`To l))
    | "cc"                -> p_address_list     (fun l -> p_crlf @@ p (`Cc l))
    | "bcc"               -> p_bcc              (fun l -> p_crlf @@ p (`Bcc l))
    | "date"              -> p_date_time        (fun d -> p_crlf @@ p (`Date d))
    | "message-id"        -> p_msg_id           (fun m -> p_crlf @@ p (`MessageID m))
    | "subject"           -> p_unstructured     (fun s -> p_crlf @@ p (`Subject s))
    | "comments"          -> p_unstructured     (fun s -> p_crlf @@ p (`Comments s))
    | "keywords"          -> p_keywords         (fun l -> p_crlf @@ p (`Keywords l))
    | "in-reply-to"       -> p_phrase_or_msg_id (fun l -> p_crlf @@ p (`InReplyTo l))
    | "resent-date"       -> p_date_time        (fun d -> p_crlf @@ p (`ResentDate d))
    | "resent-from"       -> p_mailbox_list     (fun l -> p_crlf @@ p (`ResentFrom l))
    | "resent-sender"     -> p_mailbox          (fun m -> p_crlf @@ p (`ResentSender m))
    | "resent-to"         -> p_address_list     (fun l -> p_crlf @@ p (`ResentTo l))
    | "resent-cc"         -> p_address_list     (fun l -> p_crlf @@ p (`ResentCc l))
    | "resent-bcc"        -> p_bcc              (fun l -> p_crlf @@ p (`ResentBcc l))
    | "resent-message-id" -> p_msg_id           (fun m -> p_crlf @@ p (`ResentMessageID m))
    | "references"        -> p_phrase_or_msg_id (fun l -> p_crlf @@ p (`References l))
    | "received"          -> p_received         (fun r -> p_crlf @@ p (`Received r))
    | "return-path"       -> p_path             (fun a -> p_crlf @@ p (`ReturnPath a))
    | field               -> p_unstructured @@ (fun data -> p_crlf @@ (p (`Field (field, data))))
  in

  rule state

let p_header p state =
  let rec loop acc state =
    Lexer.p_try_rule
      (fun field -> loop (field :: acc)) (p (List.rev acc))
      (p_field (fun data state -> `Ok (data, state)))
      state
  in

  loop [] state
