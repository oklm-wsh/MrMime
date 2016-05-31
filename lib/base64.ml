open BaseLexer

module T =
struct
  let _to =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  type t =
    {
      buffer       : Bytes.t;
      mutable seek : int;
      mutable cnum : int;
      wrap         : bool;
    }

  let make ?(wrap = true) () =
    { buffer = Bytes.make 2 (Char.chr 255)
    ; seek = 0
    ; cnum = 0
    ; wrap }

  let wrap t buf =
    t.cnum <-
      if t.cnum + 4 > 76
      then (Buffer.add_char buf '\n'; 0)
      else t.cnum + 4

  let add t chr buf =
    if t.seek >= 2
    then begin
      assert (t.seek = 2);

      let a, b, c = t.buffer.[0], t.buffer.[1], chr in
      if t.wrap then wrap t buf;

      t.seek <- 0;

      let quantum =
        ((Char.code a) lsl 16) +
        ((Char.code b) lsl 8 ) +
        ((Char.code c))
      in

      let a =  quantum lsr 18 in
      let b = (quantum lsr 12) land 63 in
      let c = (quantum lsr 6 ) land 63 in
      let d =  quantum         land 63 in
      Buffer.add_char buf _to.[a];
      Buffer.add_char buf _to.[b];
      Buffer.add_char buf _to.[c];
      Buffer.add_char buf _to.[d]
    end else begin
      Bytes.set t.buffer t.seek chr;
      t.seek <- t.seek + 1
    end;

    t

  let flush t buf =
    if t.wrap then wrap t buf;
    match t.seek with
    | 2 ->
      let b, c = t.buffer.[0], t.buffer.[1] in

      t.seek <- 0;

      let quantum =
        ((Char.code b) lsl 10) +
        ((Char.code c) lsl 2 )
      in

      let b = (quantum lsr 12) land 63 in
      let c = (quantum lsr 6 ) land 63 in
      let d =  quantum         land 63 in
      Buffer.add_char buf (_to.[b]);
      Buffer.add_char buf (_to.[c]);
      Buffer.add_char buf (_to.[d]);
      Buffer.add_char buf '='
    | 1 ->
      let c = t.buffer.[0] in

      t.seek <- 0;

      let quantum =
        ((Char.code c) lsl 4)
      in

      let c = (quantum lsr 6) land 63 in
      let d =  quantum        land 63 in
      Buffer.add_char buf (_to.[c]);
      Buffer.add_char buf (_to.[d]);
      Buffer.add_char buf '=';
      Buffer.add_char buf '='
    | 0 -> ()
    | _ -> assert false
end

module F =
struct
  type t = int * int
  (* quantum x size *)

  let _of =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
     \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
     \255\255\255\255\255\255\255\255\255\255\255\062\255\255\255\063\
     \052\053\054\055\056\057\058\059\060\061\255\255\255\255\255\255\
     \255\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\
     \015\016\017\018\019\020\021\022\023\024\025\255\255\255\255\255\
     \255\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\
     \041\042\043\044\045\046\047\048\049\050\051\255\255\255\255\255"

  (* XXX: paranoid mode *)
  let () =
    String.iteri
      (fun idx chr ->
        if idx = Char.code _of.[Char.code chr]
        then ()
        else assert false)
      T._to

  let default = (0, 0)

  let add (quantum, size) chr buf =
    let code = Char.code (_of.[Char.code chr]) in

    assert (code < 64);

    match size with
    | 0 -> (code, 1)
    | 1 -> ((quantum lsl 6) lor code, 2)
    | 2 -> ((quantum lsl 6) lor code, 3)
    | 3 ->
      let a =  (quantum lsr 10)           land 255 in
      let b =  (quantum lsr 2)            land 255 in
      let c = ((quantum lsl 6)  lor code) land 255 in

      Buffer.add_char buf (Char.chr a);
      Buffer.add_char buf (Char.chr b);
      Buffer.add_char buf (Char.chr c);

      (0, 0)
    | _ -> assert false

  let flush (quantum, size) buf =
    match size with
    | 0 | 1 -> ()
    | 2 ->
      let quantum = quantum lsr 4 in
      Buffer.add_char buf (Char.chr (quantum land 255))
    | 3 ->
      let quantum = quantum lsr 2 in
      let a = (quantum lsr 8) land 255 in
      let b =  quantum        land 255 in
      Buffer.add_char buf (Char.chr a);
      Buffer.add_char buf (Char.chr b)
    | _ -> assert false

  let padding (quantum, size) padding =
    match size, padding with
    | 0, 0 -> true
    | 1, _ -> false
    | 2, 2 -> true
    | 3, 1 -> true
    | _    -> false
end

let p_decode stop p state =
  [%debug Printf.printf "state: p_decode (Base64)\n%!"];

  let buf = Buffer.create 16 in

  let rec to_stop = function
    | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> (to_stop[@tailcall]) @@ safe k i))
    | #Error.err as err -> err
    | `Stop state -> p (Buffer.contents buf) state
    | `Continue state -> (junk_chr @ (fun state -> (to_stop[@tailcall]) (stop state))) state
  in

  let rec decode base64 padding state =
    match peek_chr state with
    | Some (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/') as chr) ->
      [%debug Printf.printf "state: p_decode (Base64) data\n%!"];

      if padding = 0 then begin
        state.Lexer.pos <- state.Lexer.pos + 1;
        decode (F.add base64 chr buf) padding state;
      end else begin
        F.flush base64 buf;
        raise (Error.Error (Error.err_unexpected chr state))
      end
    | Some '=' ->
      [%debug Printf.printf "state: p_decode (Base64) =\n%!"];

      state.Lexer.pos <- state.Lexer.pos + 1;
      decode base64 (padding + 1) state
    | Some ('\x20' | '\x09') ->
      [%debug Printf.printf "state: p_decode (Base64) space\n%!"];

      state.Lexer.pos <- state.Lexer.pos + 1;
      decode base64 padding state
    | Some '\r' ->
      [%debug Printf.printf "state: p_decode (Base64) CLRF\n%!"];

      (p_chr '\r' @ p_chr '\n' @ decode base64 padding) state
    | Some chr ->
      [%debug Printf.printf "state: p_decode (Base64) stop\n%!"];

      F.flush base64 buf;
      if F.padding base64 padding
      then roll_back (fun state -> to_stop (stop state)) "\r\n" state
           (* XXX: this decoder consume all CRLF needed by {close_}delimiter. *)
      else raise (Error.Error (Error.err_wrong_padding state))
    | None -> p (Buffer.contents buf) state
  in

  decode F.default 0 state

let p_encode stop p state =
  let buf = Buffer.create 16 in

  let rec encode base64 state =
    let rec aux = function
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err -> err
      | `Stop state ->
        T.flush base64 buf;
        p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr
         @ fun chr -> junk_chr
         @ encode (T.add base64 chr buf))
        state
    in aux (stop state)
  in

  encode (T.make ()) state

let p_encode' ?(wrap = true) stop p state =
  let buf = Buffer.create 16 in

  let rec encode base64 state =
    let rec aux = function
      | `Read (buf, off, len, k) ->
        `Read (buf, off, len, (fun i -> aux @@ safe k i))
      | #Error.err as err -> err
      | `Stop state ->
        T.flush base64 buf;
        p (Buffer.contents buf) state
      | `Continue state ->
        (cur_chr
         @ fun chr -> junk_chr
         @ encode (T.add base64 chr buf))
        state
    in aux (stop state)
  in

  encode (T.make ~wrap ()) state

let p_encode stop p state = p_encode' ~wrap:true stop p state
let p_inline_encode stop p state = p_encode' ~wrap:false stop p state
