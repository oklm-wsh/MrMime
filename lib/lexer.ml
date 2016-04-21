(* let () = Logs.set_level ~all:true (Some Logs.Debug) *)
let () = Logs.set_reporter (Logs_fmt.reporter ())

type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

type error =
  [ `Unexpected_eoi
  | `Expected_char   of char
  | `Expected_set    of char list
  | `Unexpected_char of char
  | `Unexpected_str  of string ]

let err e state                  = `Error (e, state.buffer, state.pos, state.len)
let err_unexpected_eoi state     = err `Unexpected_eoi state
let err_expected chr state       = err (`Expected_char chr) state
let err_expected_set set state   = err (`Expected_set set) state
let err_unexpected chr state     = err (`Unexpected_char chr) state
let err_unexpected_str str state = err (`Unexpected_str str) state

let p = Format.fprintf

let pp_lst ?(sep = "") pp_data fmt set =
  let rec aux = function
    | [] -> ()
    | [ x ] -> p fmt "%a" pp_data x
    | x :: r -> p fmt "%a%s" pp_data x sep; aux r
  in
  aux set

let pp_char fmt = p fmt "%c"

let pp_error fmt = function
  | `Unexpected_eoi      -> p fmt "Unexpected EOI"
  | `Expected_char chr   -> p fmt "Expected [%S]" (String.make 1 chr)
  | `Expected_set set    -> p fmt "Expected [%a]" (pp_lst ~sep:" | " pp_char) set
  | `Unexpected_char chr -> p fmt "Unexpected [%S]" (String.make 1 chr)
  | `Unexpected_str str  -> p fmt "Unexpected [%S]" str

type e = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

exception Error of e

let safe k state =
  try k state
  with Error (`Error (err, s, i, l)) -> `Error (err, s, i, l)

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
  (Logs.debug @@ fun m -> m "state: p_roll_back");

  let len = Bytes.length data in
  if state.pos > len
  then begin
    for i = 0 to len - 1
    do Bytes.set state.buffer (state.pos - 1 - i) (Bytes.get data (len - 1 - i)) done;
    state.pos <- state.pos - len;

    safe k state
  end else begin
    let new_len    = state.len - state.pos + len in
    let new_buffer = Bytes.create new_len in
    Bytes.blit data 0 new_buffer 0 len;
    Bytes.blit state.buffer state.pos new_buffer len (state.len - state.pos);

    (Logs.debug @@ fun m -> m "state: p_roll_back (new buffer %S)" new_buffer);

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
  else raise (Error (err_unexpected_eoi state))

let junk_chr state =
  if state.pos < state.len
  then state.pos <- state.pos + 1
  else raise (Error (err_unexpected_eoi state))

let p_chr chr state =
  (Logs.debug @@ fun m -> m "state: p_chr [%S]" (String.make 1 chr));

  match peek_chr state with
  | Some c when chr = c ->
    junk_chr state
  | Some _ ->
    raise (Error (err_expected chr state))
  | None ->
    raise (Error (err_unexpected_eoi state))

let p_set l state =
  (Logs.debug @@ fun m -> m "state: p_lst");

  match peek_chr state with
  | Some c when List.exists ((=) c) l ->
    junk_chr state
  | Some _ ->
    raise (Error (err_expected_set l state))
  | None ->
    raise (Error (err_unexpected_eoi state))

let p_while f state =
  let i0 = state.pos in
  while state.pos < state.len && f (Bytes.get state.buffer state.pos)
  do state.pos <- state.pos + 1 done;
  if i0 < state.pos
  then Bytes.sub state.buffer i0 (state.pos - i0)
  else raise (Error (err_unexpected (cur_chr state) state))

let make ?(len = 1024) () =
  { buffer = Bytes.create len
  ; pos    = 0
  ; len    = 0 }

let of_string str =
  { buffer = Bytes.of_string str
  ; pos    = 0
  ; len    = String.length str }

let of_bytes bytes =
  { buffer = bytes
  ; pos    = 0
  ; len    = Bytes.length bytes }

let p_try_rule success fail rule state =
  let tmp = Buffer.create 16 in

  Buffer.add_bytes tmp (Bytes.sub state.buffer state.pos (state.len - state.pos));

  let rec loop = function
    | `Error (_, buf, off, len) -> safe fail (of_string (Buffer.contents tmp))
    | `Read (buf, off, len, k) ->
      `Read (buf, off, len,
        (fun writing ->
         Buffer.add_bytes tmp (Bytes.sub buf off writing);
         loop @@ safe k writing))
    | `Ok (data, state) -> safe (success data) state
  in

  loop @@ safe rule state
