open BaseDecoder

module T =
struct
  open BaseEncoder

  let _to =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  type t =
    { state        : Encoder.t
    ; buffer       : Bytes.t
    ; mutable seek : int
    ; mutable cnum : int
    ; wrap         : bool }

  let lift ?(wrap = true) k state =
    k { state
      ; buffer = Bytes.make 2 (Char.chr 255)
      ; seek   = 0
      ; cnum   = 0
      ; wrap }

  let wrap k ({ state; cnum; _ } as t) =
    if cnum + 4 > 76
    then w "\r\n" (fun state -> k { t with state = state; cnum = 4; }) state
    else k { t with cnum = cnum + 4 }

  let add chr k ({ state; buffer; seek; _ } as t) =
    if seek >= 2
    then begin
      let a, b, c = buffer.[0], buffer.[1], chr in
      (if t.wrap
       then wrap
       else noop)
      (fun ({ state; _ } as t) ->
       let quantum =
         ((Char.code a) lsl 16) +
         ((Char.code b) lsl 8 ) +
         ((Char.code c))
       in

       let a =  quantum lsr 18 in
       let b = (quantum lsr 12) land 63 in
       let c = (quantum lsr 6 ) land 63 in
       let d =  quantum         land 63 in

       (w_char _to.[a]
        $ w_char _to.[b]
        $ w_char _to.[c]
        $ w_char _to.[d])
       (fun state -> k { t with state = state; seek = 0 })
       state)
      t
    end else begin
      Bytes.set t.buffer t.seek chr;
      k { t with seek = seek + 1; }
    end

  let flush k t =
    (if t.wrap
     then wrap
     else noop)
    (fun ({ state; buffer; seek; _ } as t) ->
     match seek with
     | 2 ->
       let b, c = buffer.[0], buffer.[1] in

       t.seek <- 0;

       let quantum =
         ((Char.code b) lsl 10) +
         ((Char.code c) lsl 2 )
       in

       let b = (quantum lsr 12) land 63 in
       let c = (quantum lsr 6 ) land 63 in
       let d =  quantum         land 63 in

       (w_char _to.[b]
        $ w_char _to.[c]
        $ w_char _to.[d]
        $ w_char '=')
       (fun state -> k { t with state = state; seek = 0 })
       state
     | 1 ->
       let c = buffer.[0] in

       t.seek <- 0;

       let quantum =
         ((Char.code c) lsl 4)
       in

       let c = (quantum lsr 6) land 63 in
       let d =  quantum        land 63 in

       (w_char _to.[c]
        $ w_char _to.[d]
        $ w_char '='
        $ w_char '=')
       (fun state -> k { t with state = state; seek = 0 })
       state
     | 0 -> k t
     | _ -> assert false)
    t

  let unlift k t =
    flush (fun { state; _ } -> k state) t
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
        `Read (buf, off, len, (fun i -> to_stop @@ safe k i))
    | #Error.err as err -> err
    | `Stop state -> p (Buffer.contents buf) state
    | `Continue state -> (junk_chr @ (fun state -> to_stop (stop state))) state
  in

  let rec decode base64 padding state =
    match peek_chr state with
    | Some (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/') as chr) ->
      [%debug Printf.printf "state: p_decode (Base64) data\n%!"];

      if padding = 0 then begin
        state.Decoder.pos <- state.Decoder.pos + 1;
        (decode[@tailcall]) (F.add base64 chr buf) padding state;
      end else begin
        F.flush base64 buf;
        raise (Error.Error (Error.err_unexpected chr state))
      end
    | Some '=' ->
      [%debug Printf.printf "state: p_decode (Base64) =\n%!"];

      state.Decoder.pos <- state.Decoder.pos + 1;
      (decode[@tailcall]) base64 (padding + 1) state
    | Some ('\x20' | '\x09') ->
      [%debug Printf.printf "state: p_decode (Base64) space\n%!"];

      state.Decoder.pos <- state.Decoder.pos + 1;
      (decode[@tailcall]) base64 padding state
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

open BaseEncoder

let explode str =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (str.[i] :: l) in
  exp (String.length str - 1) []

let w_inline_encode str k =
  T.lift ~wrap:false
    (List.fold_right T.add (explode str) (T.unlift k))

let w_encode content k =
  let len = String.length content in

  let rec loop idx k =
    if idx < len
    then T.add (String.get content idx) (fun state -> (loop[@tailcall]) (idx + 1) k state)
    else k
  in

  T.lift (loop 0 @@ T.unlift k)
