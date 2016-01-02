(* TODO: comments with RFC 2045 § 6.8 *)
(* TODO: optimize *)
{
  module T =
  struct
    let _to =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    type t =
      {
        buffer       : Bytes.t;
        mutable seek : int;
        mutable cnum : int;
      }

    let make () =
      (* XXX: or default? (like module F) *)
      { buffer = Bytes.make 2 (Char.chr 255)
      ; seek = 0
      ; cnum = 0 }

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
        wrap t buf;

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
      wrap t buf;
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

  exception Unexpected_character of char
  exception Wrong_padding

  let explode str = Array.init (String.length str) (String.get str)
}

let b64_chr = ['A' - 'Z'] | ['a' - 'z'] | ['0' - '9'] | '+' | '/'
let b64_pad = '='
let b64_wsp = ' ' | '\t'
(* from LWSP_char of RFC 822 (see lexer.ml) *)
let cr      = '\013'
let lf      = '\010'
let crlf    = (cr lf) | lf
(* from CRLF of RFC 822 (see lexer.ml)
   with support of non-conforming e-mails *)

rule decode buf acc padding = parse
  | b64_chr as chr
    { if padding = 0
      then decode buf (F.add acc chr buf) padding lexbuf
      else begin F.flush acc buf; raise (Unexpected_character chr) end }
  | b64_pad
    { decode buf acc (padding + 1) lexbuf }
  | (crlf | b64_wsp) *
    { decode buf acc padding lexbuf }
  | eof
    {  F.flush acc buf;
      if F.padding acc padding
      then ()
      else raise Wrong_padding }
  (* XXX: should be ignored *)
  | _ as chr
    { raise (Unexpected_character chr) }

and  encode buf acc = parse
  | eof { T.flush acc buf }
  | _ as chr
    { encode buf (T.add acc chr buf) lexbuf }

{
  let decode buf lexbuf =
    decode buf F.default 0 lexbuf

  let encode buf lexbuf =
    encode buf (T.make ()) lexbuf

  module Encode = Flow.Make(struct let conv = encode end)
  module Decode = Flow.Make(struct let conv = decode end)
}
