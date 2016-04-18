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

type date = int * month * int
type time = int * int * int option

type atom   = [ `Atom of string ]
type word   = [ atom | `String of string ]
type phrase = [ word | `Dot | `FWS ] list

type domain =
  [ `Literal of string
  | `Domain of atom list ]

type local   = word list
type person  = phrase option * (local * domain list)
type group   = phrase * person list
type address = [ `Group of group | `Person of person ]

let cur_chr ?(avoid = []) state =
  while List.exists ((=) (Lexer.cur_chr state)) avoid
  do state.Lexer.pos <- state.Lexer.pos + 1 done;

  Lexer.cur_chr state

let p_try f state =
  let i0 = state.Lexer.pos in
  while state.Lexer.pos < state.Lexer.len
     && f (Bytes.get state.Lexer.buffer state.Lexer.pos)
  do state.Lexer.pos <- state.Lexer.pos + 1 done;

  let n = state.Lexer.pos - i0 in
  state.Lexer.pos <- i0;
  n

let p_try_rule success fail rule state =
  let tmp = Buffer.create 16 in

  Buffer.add_bytes tmp
    (Bytes.sub state.Lexer.buffer state.Lexer.pos (state.Lexer.len - state.Lexer.pos));
  (Logs.debug @@ fun m -> m "state: try_rule (save data %S)" (Buffer.contents tmp));

  let rec loop = function
    | `Error (_, buf, off, len) ->
      (* Buffer.add_bytes tmp (Bytes.sub buf off (len - off)); *)
      (Logs.debug @@ fun m -> m "state: try_rule/fail");

      (Logs.debug @@ fun m -> m "state: try_rule (new buffer %S)" (Buffer.contents tmp));

      Lexer.safe fail (Lexer.of_string (Buffer.contents tmp))
    | `Read (buf, off, len, k) ->
      `Read (buf, off, len,
        (fun writing ->
         Buffer.add_bytes tmp (Bytes.sub buf off writing);
         (Logs.debug @@ fun m -> m "sate: try_rule (save data %S)" (Buffer.contents tmp));
         loop @@ Lexer.safe k writing))
    | `Ok (data, state) -> Lexer.safe (success data) state
  in

  loop @@ Lexer.safe rule state

let p_repeat ?a ?b f state =
  let i0 = state.Lexer.pos in
  let most pos = match b with
    | Some most -> (pos - i0) <= most
    | None -> true
  in
  let least pos = match a with
    | Some least -> (pos - i0) >= least
    | None -> true
  in
  while state.Lexer.pos < state.Lexer.len
        && f (Bytes.get state.Lexer.buffer state.Lexer.pos)
        && most state.Lexer.pos
  do state.Lexer.pos <- state.Lexer.pos + 1 done;
  if least state.Lexer.pos
  then Bytes.sub state.Lexer.buffer i0 (state.Lexer.pos - i0)
  else raise (Lexer.Error (Lexer.err_unexpected (cur_chr state) state))

let is_obs_no_ws_ctl = function
  | '\000' .. '\008'
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
   SP              = %x20
   HTAB            = %x09                 ; horizontal tab
   WSP             = SP / HTAB            ; white space
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

let is_wsp = function
  | '\x20' | '\x09' -> true
  | chr             -> false
let s_wsp  = ['\x20'; '\x09']

(* See RFC 5322 § 3.2.1:

   quoted-pair     = ("\" (VCHAR / WSP)) / obs-qp)
   obs-qp          = "\" (%d0 / obs-NO-WS-CTL / LF / CR)"
*)
let of_escaped_character = function
  | 'a' -> '\x07'
  | 'b' -> '\b'
  | 't' -> '\t'
  | 'n' -> '\n'
  | 'v' -> '\x0b'
  | 'f' -> '\x0c'
  | 'r' -> '\r'
  | chr -> chr

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

   XXX: it's [FWS], not FWS!
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

  (* for *(CRLF 1*WSP) loop  *)
  let rec end_of_line has_fws success fail state =
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
           success true state
         (* … *(CRLF 1*WSP) CRLF e *)
         | chr ->
           (Logs.debug @@ fun m -> m "state: p_fws/rollback");
           Lexer.roll_back (p true) tmp state)
        state
    (* … e *)
    | None -> fail has_fws state
  in

  let rec loop state =
    match cur_chr state with
    (* WSP / CR *)
    | '\x20' | '\x09' | '\r' as chr ->
      let success has_fws state =
        match chr with
        (* 1*WSP *(CRLF 1*WSP), so it's obs-fws *)
        | '\x20' | '\x09' ->
          (* it's 1*WSP 1*(CRLF 1*WSP) loop:
             we can have multiple CRLF 1*WSP because we are in obs-fws *)
          let rec loop _ state =
            end_of_line true loop (fun _ -> p true) state in

          loop true state
        (* 1*(CRLF 1*WSP) *)
        | chr -> p true state
      in
      let fail has_fws state =
        (* WSP / CR *)
        match chr with
        | '\r' -> (* CR XXX: don't drop '\r', may be it's \r\n *)
          p true state
        | chr  -> (* we have 1*WSP, so we drop 1*WSP *)
          let _ = Lexer.p_while is_wsp state in p true state
      in

      (* state (WSP / CR) and try [*WSP CRLF] 1*WSP *)
      end_of_line false success fail state
    (* no fws *)
    | chr -> p false state
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
      p_fws (fun _ -> p_ccontent @@ p_fws @@ (fun _ -> loop)) state
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

      p_comment (p_fws (fun has_fws' -> loop (has_fws || has_fws') true)) state
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

  p_fws (fun has_fws -> loop has_fws false) state

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

(* See RFC 3233 § 3.2.3:

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
  Lexer.p_while is_atext state

let p_atom p =
  (Logs.debug @@ fun m -> m "state: p_atom");

  p_cfws
  @@ fun _ state -> let atext = p_atext state in p_cfws (fun _ -> p atext) state

let p_dot_atom_text p state =
  (Logs.debug @@ fun m -> m "state: p_dot_atom_text");

  let rec next acc state =
    (Logs.debug @@ fun m -> m "state: p_dot_atom_text/next");

    match cur_chr state with
    | '.' -> Lexer.junk_chr state; next (`Atom (p_atext state) :: acc) state
    | chr -> p (List.rev acc) state
  in

  next [`Atom (p_atext state)] state

let p_dot_atom p =
  (Logs.debug @@ fun m -> m "state: p_dot_atom");

  (* [CFWS]              dot-atom-text               [CFWS] *)
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

let p_qcontent p state =
  (Logs.debug @@ fun m -> m "state: p_qcontent");

  match cur_chr state with
  | '\\' -> p_quoted_pair (fun chr -> p (String.make 1 chr)) state
  | chr  -> p (p_qtext state) state

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
         p_fws (fun has_fws ->
                (Logs.debug @@ fun m -> m "has_fws: %b" has_fws);
                loop (if has_fws then " " :: str :: acc else str :: acc)))
        state
  in
  p_cfws (fun _ state -> Lexer.p_chr '"' state; loop [] state) state

let p_word p state =
  (Logs.debug @@ fun m -> m "state: p_word");

  let loop has_fws state =
    match cur_chr state with
    | '"' ->
      (Logs.debug @@ fun m -> m "state: p_word/loop to p_quoted_string [%S]" (String.make 1 '"'));
      p_quoted_string (fun s -> p (`String s)) state
    | chr ->
      (Logs.debug @@ fun m -> m "state: p_word/loop to p_atom [%S]" (String.make 1 chr));
      p_atom (fun s -> p (`Atom s)) state
  in

  p_cfws (fun has_fws -> loop has_fws) state

let p_phrase p state =
  (Logs.debug @@ fun m -> m "state: p_phrase");

  let add_fws has_fws element words =
    if has_fws
    then element :: `FWS :: words
    else element :: words
  in

  (* XXX: remove unused FWS, if we don't remove that, the pretty-printer raise
          an error. May be, we fix that in the pretty-printer but I decide to
          fix that in this place. *)
  let rec delete_last_fws = function
    | `FWS :: r -> delete_last_fws r
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
            | _ -> p (List.rev @@ delete_last_fws words) state)
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
            | _ -> obs (if has_fws then `FWS :: words else words) state)
      state
  in

  p_word (fun word -> loop [word]) state

let is_obs_utext = function
  | '\000' -> true
  | chr -> is_obs_no_ws_ctl chr || is_vchar chr

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
  p_fws (fun _ state -> loop0 state) state

let p_unstructured p state =
  let rec loop state =
    (* [FWS] *)
    p_fws (fun has_fws state -> match has_fws, cur_chr state with
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

(* [CFWS] 2DIGIT [CFWS] *)
let p_cfws_2digit_cfws p state =
  (Logs.debug @@ fun m -> m "state: p_cfws_2digit_cfws");

  p_cfws (fun _ state -> let n = p_repeat ~a:2 ~b:2 is_digit state in
                       p_cfws (p (int_of_string n)) state) state

let p_obs_hour p state =
  (Logs.debug @@ fun m -> m "state: p_obs_hour");
  p_cfws_2digit_cfws (fun n _ -> p n) state

let p_obs_minute p state =
  (Logs.debug @@ fun m -> m "state: p_obs_minute");
  p_cfws_2digit_cfws (fun n _ -> p n) state

let p_obs_second p state =
  (Logs.debug @@ fun m -> m "state: p_obs_second");
  p_cfws_2digit_cfws p state

let p_2digit_or_obs p state =
  (Logs.debug @@ fun m -> m "state: p_2digit_or_obs");

  if p_try is_digit state = 2
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

let p_obs_year p state =
  (* [CFWS] 2*DIGIT [CFWS] *)
  p_cfws (fun _ state -> let y = p_repeat ~a:2 is_digit state in
                         p_cfws (fun _ -> p (int_of_string y)) state) state
let p_year has_already_fws p state =
  (* (FWS 4*DIGIT FWS) / obs-year *)
  p_fws (fun has_fws state ->
    if (has_fws || has_already_fws) && p_try is_digit state >= 4
    then let y = Lexer.p_while is_digit state in
         p_fws (fun has_fws state ->
                if has_fws
                then p (int_of_string y) state
                else raise (Lexer.Error (Lexer.err_expected ' ' state))) state
    else p_obs_year p state)
  state

let p_obs_day p state =
  p_cfws (fun _ state -> let d = p_repeat ~a:1 ~b:2 is_digit state in
                         p_cfws (fun _ -> p (int_of_string d)) state)
    state

let p_day p state =
  p_fws (fun _ state ->
         if is_digit (cur_chr state)
         then let d = p_repeat ~a:1 ~b:2 is_digit state in
              p_fws (fun has_fws ->
                     if has_fws
                     then p (int_of_string d)
                     else raise (Lexer.Error (Lexer.err_expected ' ' state))) state
         else p_obs_day p state)
    state

let p_month p state =
  let month = p_repeat ~a:3 ~b:3 is_alpha state in

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

let p_day_name p state =
  let day = p_repeat ~a:3 ~b:3 is_alpha state in

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

let p_day_of_week p =
  p_fws
  @@ fun _ state ->
     if is_alpha (cur_chr state) then p_day_name p state
     else p_cfws (fun _ -> p_day_name (fun day -> p_cfws (fun _ -> p day)))
            state

let p_date p =
  p_day (fun d -> p_month (fun m -> p_year false (fun y -> p (d, m, y))))

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

let p_obs_zone p state =
  let k x = p x state in
  match cur_chr state with
  | '\065' .. '\073' ->
    let a = cur_chr state in
    Lexer.junk_chr state;

    if a = 'G' || a = 'E' || a = 'C'
       && (cur_chr state = 'M' || cur_chr state = 'S' || cur_chr state = 'D')
    then let next = p_repeat ~a:2 ~b:2 is_alpha state in
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
    then let next = p_repeat ~a:2 ~b:2 is_alpha state in
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

let p_zone has_already_fws p state =
  (Logs.debug @@ fun m -> m "state: p_zone %b" has_already_fws);

  p_fws (fun has_fws state ->
         match has_already_fws || has_fws, cur_chr state with
         | true, '+' ->
           Lexer.p_chr '+' state;
           let tz = p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (int_of_string tz)) state
         | true, '-' ->
           Lexer.p_chr '-' state;
           let tz = p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (- (int_of_string tz))) state
         | true, chr when is_digit chr ->
           let tz = p_repeat ~a:4 ~b:4 is_digit state in
           p (`TZ (int_of_string tz)) state
         | _ -> p_obs_zone p state)
    state

let p_time p state =
  (Logs.debug @@ fun m -> m "state: p_time");

  p_time_of_day
    (fun has_fws (hh, mm, dd) -> p_zone has_fws (fun tz -> p ((hh, mm, dd), tz)))
    state

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

  if is_alpha @@ cur_chr state
  then p_day_of_week (fun day state -> Lexer.p_chr ',' state; aux ~day state) state
  else aux state

(* See RFC 5322 § 3.4.1:

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
    | chr ->
      p_dtext (fun s -> p_fws (fun _ -> loop (s :: acc))) state
  in

  p_cfws (fun _ state ->
          match cur_chr state with
          | '[' -> Lexer.p_chr '[' state; p_fws (fun _ -> loop []) state
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
      p_try_rule
        (function
          | [] -> p_obs_local_part p
          | l  -> p_obs_local_part' (List.rev l))
        (p_obs_local_part p)
        (p_dot_atom (fun l state -> `Ok (l, state)))
        state)

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

  p_try_rule p (p_obs_angle_addr p)
    (first (fun data state -> `Ok (data, state)))
    state

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

let p_mailbox p state =
  (Logs.debug @@ fun m -> m "state: p_mailbox");

  p_try_rule
    p
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

      p_try_rule
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

      p_try_rule
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
  p_try_rule
    (fun data -> p data)
    (p_try_rule
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
      match cur_chr state with
      | ';' -> Lexer.p_chr ';' state; p_cfws (fun _ -> p (display_name, [])) state
      | chr -> p_group_list (fun group -> p_cfws (fun _ -> p (display_name, group))) state)
    state

let p_address p state =
  (Logs.debug @@ fun m -> m "state: p_address");

  p_try_rule
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

      p_try_rule
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

      p_try_rule
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
      p_try_rule
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
