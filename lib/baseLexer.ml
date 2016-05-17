open Lexer

let safe k state =
  try k state
  with Error.Error (`Error (err, s, i, l)) -> `Error (err, s, i, l)

let read_exact need k state =
  let tmp  = Bytes.create need in
  let have = min need (state.len - state.pos) in
  Bytes.blit state.buffer state.pos tmp 0 have;
  state.pos <- state.pos + have;
  let rec loop rest =
    if rest = 0 then
      safe (k tmp) state
    else
      `Read (tmp, state.pos, need - rest, fun m -> loop (rest - m))
  in
  loop (need - have)

let has_line buff off len =
  let rec loop cr_read j =
    if j >= len then false
    else
      match Bytes.get buff j with
      | '\n' ->
        if cr_read then true else loop false (j + 1)
      | '\r' -> loop true (j + 1)
      | _ -> loop false (j + 1)
  in
  loop false off

let rec roll_back k data state =
  let len = Bytes.length data in
  if state.pos > len
  then begin
    for i = 0 to len - 1
    do Bytes.set state.buffer
         (state.pos - 1 - i)
         (Bytes.get data (len - 1 - i))
    done;
    state.pos <- state.pos - len;

    safe k state
  end else begin
    let new_len    = state.len - state.pos + len in
    let new_buffer = Bytes.create new_len in
    Bytes.blit data 0 new_buffer 0 len;
    Bytes.blit state.buffer state.pos new_buffer len (state.len - state.pos);

    state.pos <- 0;
    state.len <- new_len;
    state.buffer <- new_buffer;

    safe k state
  end

let read_line k state =
  if has_line state.buffer state.pos state.len
  then k state
  else begin
    if state.pos > 0
    then begin
      Bytes.blit state.buffer state.pos state.buffer 0 (state.len - state.pos);
      state.len <- state.len - state.pos;
      state.pos <- 0
    end;

    let rec loop off =
      if has_line state.buffer state.pos off
      then begin
        state.len <- off;
        safe k state
      end else begin
        if off >= Bytes.length state.buffer
        then begin
          let new_buffer = Bytes.create (2 * Bytes.length state.buffer + 1) in
          Bytes.blit state.buffer 0 new_buffer 0 (Bytes.length state.buffer);
          state.buffer <- new_buffer
        end;

        `Read (state.buffer, off,
               Bytes.length state.buffer - off,
               fun n -> loop (off + n))
      end
    in

    loop state.len
  end

let peek_chr state =
  if state.pos < state.len
  then Some (Bytes.get state.buffer state.pos)
  else None

let cur_chr state =
  if state.pos < state.len
  then Bytes.get state.buffer state.pos
  else raise (Error.Error (Error.err_unexpected_eoi state))

let junk_chr state =
  if state.pos < state.len
  then state.pos <- state.pos + 1
  else raise (Error.Error (Error.err_unexpected_eoi state))

let p_chr chr state =
  match peek_chr state with
  | Some c when chr = c ->
    junk_chr state
  | Some _ ->
    raise (Error.Error (Error.err_expected chr state))
  | None ->
    raise (Error.Error (Error.err_unexpected_eoi state))

let p_set l state =
  match peek_chr state with
  | Some c when List.exists ((=) c) l ->
    junk_chr state
  | Some _ ->
    raise (Error.Error (Error.err_expected_set l state))
  | None ->
    raise (Error.Error (Error.err_unexpected_eoi state))

let p_str str state =
  String.iter (fun chr -> p_chr chr state) str

let p_while f state =
  let i0 = state.pos in
  while state.pos < state.len && f (Bytes.get state.buffer state.pos)
  do state.pos <- state.pos + 1 done;
  if i0 < state.pos
  then Bytes.sub state.buffer i0 (state.pos - i0)
  else raise (Error.Error (Error.err_unexpected (cur_chr state) state))

let p_try_rule success fail rule state =
  let tmp = Buffer.create 16 in

  Buffer.add_bytes tmp
    (Bytes.sub state.buffer state.pos (state.len - state.pos));

  let rec loop = function
    | `Error (err, buf, off, len) ->
      safe fail (of_string (Buffer.contents tmp))
    | `Read (buf, off, len, k) ->
      `Read (buf, off, len,
        (fun writing ->
         Buffer.add_bytes tmp (Bytes.sub buf off writing);
         loop @@ safe k writing))
    | `Ok (data, state) -> safe (success data) state
  in

  loop @@ safe rule state

(* See RFC 5234 § 3.6:

   The operator "*" preceding an element indicates repetition. The full form is:

     <a>*<b>element

   where <a> and <b> are optional decimal values, indicating at least <a> and at
   most <b> occurrences of the element.

   Default values are  0  and  infinity  so  that  *<element> allows any number,
   including  zero;  1*<element>  requires  at  least  one;  3*3<element> allows
   exactly 3; and 1*2<element> allows one or two.
*)
let p_repeat ?a ?b f state =
  let i0 = state.pos in
  let most pos = match b with
    | Some most -> (pos - i0) <= most
    | None -> true
  in
  let least pos = match a with
    | Some least -> (pos - i0) >= least
    | None -> true
  in
  while state.pos < state.len
        && f (Bytes.get state.buffer state.pos)
        && most state.pos
  do state.pos <- state.pos + 1 done;
  if least state.pos
  then Bytes.sub state.buffer i0 (state.pos - i0)
  else raise (Error.Error (Error.err_unexpected (cur_chr state) state))

let p_try f state =
  let i0 = state.pos in
  while state.pos < state.len
     && f (Bytes.get state.buffer state.pos)
  do state.pos <- state.pos + 1 done;

  let n = state.pos - i0 in
  state.pos <- i0; n
