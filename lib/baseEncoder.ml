open Encoder

let flush p state =
  if state.pos > 0
  then let rec next n =
         if n < state.pos
         then `Partial (state.buffer, n, state.pos - n, fun m -> next (n + m))
         else (state.pos <- 0; p state)
       in next 0
  else p state

let wait k state = `Wait k

let rec writes s k state =
  let len = String.length state.buffer in
  let rec loop j l state =
    let rem = len - state.pos in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j state.buffer state.pos len;
    state.pos <- state.pos + len;
    if len < l then flush (loop (j + len) (l - len)) state else k state
  in
  loop 0 (String.length s) state

let w s k e = writes s k e
let w_char chr k e = w (String.make 1 chr) k e
let noop k e = k e

let spaces = String.make 80 ' '

let wrapping fmt =
  let out_functions = Format.pp_get_formatter_out_functions fmt () in
  { out_functions with
    Format.out_newline = (fun () -> out_functions.Format.out_string "\r\n" 0 2);
    Format.out_spaces  = (fun n  ->
      if n > 0 then
      if n <= 80 then out_functions.Format.out_string spaces 0 n
      else begin
        let i = ref n in
        while !i > 80 do out_functions.Format.out_string spaces 0 80;
                         i := !i - 80; done;
        out_functions.Format.out_string spaces 0 !i
      end) }
  |> Format.pp_set_formatter_out_functions fmt

let sp = Format.sprintf

let ( & ) x y k e = x (w " " (y k)) e
let ( $ ) x y k e = x (y k) e
