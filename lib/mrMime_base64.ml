type result = [ `Dirty of string | `Clean of string | `Wrong_padding ]

open Parser
open Parser.Convenience

module F =
struct
  type t = { mutable contents : int * int } (* quantum x size *)

  let table =
    "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
     \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
     \255\255\255\255\255\255\255\255\255\255\255\062\255\255\255\063\
     \052\053\054\055\056\057\058\059\060\061\255\255\255\255\255\255\
     \255\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\
     \015\016\017\018\019\020\021\022\023\024\025\255\255\255\255\255\
     \255\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\
     \041\042\043\044\045\046\047\048\049\050\051\255\255\255\255\255"

  let make () = { contents = (0, 0) }

  let add ({ contents = (quantum, size) } as t) chr buffer =
    let code = Char.code (String.get table (Char.code chr)) in

    match size with
    | 0 -> t.contents <- (code, 1)
    | 1 -> t.contents <- ((quantum lsl 6) lor code, 2)
    | 2 -> t.contents <- ((quantum lsl 6) lor code, 3)
    | 3 ->
      let a = (quantum lsr 10) land 255 in
      let b = (quantum lsr 2)  land 255 in
      let c = ((quantum lsl 6) lor code) land 255 in

      Buffer.add_char buffer (Char.chr a);
      Buffer.add_char buffer (Char.chr b);
      Buffer.add_char buffer (Char.chr c);

      t.contents <- (0, 0)
    | _ -> assert false

  let flush { contents = (quantum, size) } buffer =
    match size with
    | 0 | 1 -> ()
    | 2 ->
      let quantum = quantum lsr 4 in
      Buffer.add_char buffer (Char.chr (quantum land 255))
    | 3 ->
      let quantum = quantum lsr 2 in
      let a = (quantum lsr 8) land 255 in
      let b =  quantum        land 255 in

      Buffer.add_char buffer (Char.chr a);
      Buffer.add_char buffer (Char.chr b)
    | _ -> assert false

  let padding { contents = (quantum, size) } padding =
    match size, padding with
    | 0, 0 -> true
    | 1, _ -> false
    | 2, 2 -> true
    | 3, 1 -> true
    | _    -> false
end

let is_b64 = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' -> true
  | _ -> false

let decode_chunk t buffer =
  { f = fun i s fail succ ->
    let decode t buff off len =
      let rec loop idx =
        if idx < len && is_b64 (Internal_buffer.get buff (off + idx))
        then (F.add t (Internal_buffer.get buff (off + idx)) buffer; loop (idx + 1))
        else idx
      in

      loop 0
    in

    let consumed = Input.transmit i (decode t) in

    succ i s consumed }

let crlf = char '\r' *> char '\n' *> return ()

let decode boundary rollback t buffer =
  let fix' f =
    let rec u a b = lazy (f r a b)
    and r a b = { f = fun i s fail succ ->
              Lazy.(force (u a b)).f i s fail succ }
    in r
  in

  (fix' @@ fun m padding dirty ->
     peek_chr >>= function
     | None ->
       F.flush t buffer;
       if F.padding t padding
       then return (match dirty with
                    | `Dirty -> false, `Dirty (Buffer.contents buffer)
                    | `Clean -> false, `Clean (Buffer.contents buffer))
       else return (false, `Wrong_padding)
     | Some chr when is_b64 chr ->
       if padding = 0
       then decode_chunk t buffer >>= fun consumed -> m padding dirty
       else (F.flush t buffer; return (false, `Dirty (Buffer.contents buffer)))
     | Some '=' ->
       advance 1 >>= fun () -> m (padding + 1) dirty
     | Some '\x20'
     | Some '\x09' ->
       advance 1 >>= fun () -> m padding dirty
     | Some '\x0D' ->
       (boundary >>= fun () ->
        F.flush t buffer;
        if F.padding t padding
        then return (match dirty with
                     | `Dirty -> true, `Dirty (Buffer.contents buffer)
                     | `Clean -> true, `Clean (Buffer.contents buffer))
        else return (true, `Wrong_padding))
       <|> (crlf >>= fun () -> m padding dirty)
     | Some chr -> advance 1 >>= fun () -> m padding `Dirty)

module Convenience =
struct
  type err += Wrong_padding

  type 'a decoder =
    { src             : 'a Input.t
    ; state           : F.t
    ; buffer          : Buffer.t
    ; mutable padding : int
    ; mutable i       : Bytes.t
    ; mutable i_off   : int
    ; mutable i_len   : int
    ; mutable k       : 'a decoder -> decode }
  and decode = [ `Continue | `Error of err | `Dirty of string | `End of string | `String of string ]

  let continue_or f { buffer; _ } =
    if Buffer.length buffer = 0
    then `Continue
    else
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      f s

  let terminate { buffer; _ } =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    `End s

  let p boundary t =
    peek_chr >>= function
    | None ->
      F.flush t.state t.buffer;
      if F.padding t.state t.padding
      then return (false, terminate t)
      else return (false, `Error Wrong_padding)
    | Some chr when is_b64 chr ->
      if t.padding = 0
      then decode_chunk t.state t.buffer
           >>| fun consumed -> (false, continue_or (fun s -> `String s) t)
      else begin
        F.flush t.state t.buffer;
        return (false, continue_or (fun s -> `Dirty s) t)
      end
    | Some '=' ->
      advance 1
      >>= fun () -> begin t.padding <- t.padding + 1;
                          return (false, `Continue)
                    end
    | Some '\x20'
    | Some '\x09' ->
      advance 1 >>= fun () -> return (false, `Continue)
    | Some '\x0D' ->
      (boundary >>= fun () ->
       begin
         F.flush t.state t.buffer;

         if F.padding t.state t.padding
         then return (true, terminate t)
         else return (true, `Error Wrong_padding)
       end)
      <|> (crlf >>= fun () -> return (false, `Continue))
    | Some chr ->
      advance 1 >>= fun () -> return (false, `Dirty (String.make 1 chr))

  let rec loop rollback t = function
    | Parser.Read { buffer; k; } ->
      t.k <- (fun t ->
              Input.write_string
                t.src
                (Bytes.unsafe_to_string t.i)
                t.i_off t.i_len;

              let s = if t.i_len = 0
                      then Parser.Complete
                      else Parser.Incomplete
              in

              loop rollback t @@ k t.i_len s);
      `Continue
    | Parser.Fail (marks, exn) -> `Error exn
    | Parser.Done (need_to_rollback, v) ->
      if need_to_rollback
      then match Parser.run t.src rollback with
           | Parser.Done () -> v
           | _ -> assert false (* XXX: rollback does not fail and does not
                                       require any input. *)
      else v

  let decoder_src t = t.src

  let decoder (rollback, boundary) src =
    { src
    ; state   = F.make ()
    ; buffer  = Buffer.create 16
    ; padding = 0
    ; i       = Bytes.empty
    ; i_off   = 0
    ; i_len   = 0
    ; k       = fun t -> loop rollback t
                         @@ Parser.run t.src (option (false, `End "") (crlf *> p boundary t)) }

  let decode t = t.k t

  let src t buf off len =
    if (off < 0 || len < 0 || off + len > Bytes.length buf)
    then raise (Invalid_argument "Base64.src");

    t.i <- buf;
    t.i_off <- off;
    t.i_len <- len;
end

let inline t buffer =
  let fix' f =
    let rec u a b = lazy (f r a b)
    and r a b = { f = fun i s fail succ ->
              Lazy.(force (u a b)).f i s fail succ }
    in r
  in

  fix' @@ fun m padding dirty ->
    peek_chr >>= function
    | None | Some '?' ->
      F.flush t buffer;
      if F.padding t padding
      then return (match dirty with
                   | `Dirty -> `Dirty (Buffer.contents buffer)
                   | `Clean -> `Clean (Buffer.contents buffer))
      else return `Wrong_padding
    | Some chr when is_b64 chr ->
      if padding = 0
      then decode_chunk t buffer >>= fun consumed -> m padding dirty
      else (F.flush t buffer; return (`Dirty (Buffer.contents buffer)))

    | Some '=' ->
      advance 1 >>= fun () -> m (padding + 1) dirty
    | Some '\x20'
    | Some '\x09' ->
      advance 1 >>= fun () -> m padding dirty
    | Some chr -> advance 1 >>= fun () -> m padding `Dirty

let decode boundary rollback =
  decode boundary rollback (F.make ()) (Buffer.create 16) 0 `Clean
  >>= function
    | true, content  -> rollback *> return content
    | false, content -> return content

let inline () =
  inline (F.make ()) (Buffer.create 16) 0 `Clean

module T =
struct
  open Encoder

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
    then string "\r\n" (fun state -> k { t with state = state; cnum = 4; }) state
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

       (char _to.[a]
        $ char _to.[b]
        $ char _to.[c]
        $ char _to.[d])
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

       (char _to.[b]
        $ char _to.[c]
        $ char _to.[d]
        $ char '=')
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

       (char _to.[c]
        $ char _to.[d]
        $ char '='
        $ char '=')
       (fun state -> k { t with state = state; seek = 0 })
       state
     | 0 -> k t
     | _ -> assert false)
    t

  let unlift k t =
    flush (fun { state; _ } -> k state) t
end

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
