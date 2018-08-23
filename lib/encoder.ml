type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int }

type 'r partial = [> `Partial of (Bytes.t * int * int * (int -> 'r)) ] as 'r

type 'r           k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)     k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r) k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

let make () =
  { buffer = Bytes.create 4096
  ; pos    = 0 }

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
  let len = Bytes.length state.buffer in
  let rec loop j l state =
    let rem = len - state.pos in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j state.buffer state.pos len;
    state.pos <- state.pos + len;
    if len < l then flush (loop (j + len) (l - len)) state else k state
  in
  loop 0 (String.length s) state

let string s k e = writes s k e
let char chr k e = string (String.make 1 chr) k e
let noop k e     = k e
let sp           = Format.sprintf

let spaces = String.make 80 ' '

let ( & ) x y k e = x (string " " (y k)) e
let ( $ ) x y k e = x (y k) e
