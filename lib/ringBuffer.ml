module type S =
sig
  type 'a t

  val create_bytes     : int -> Internal_buffer.st t
  val create_bigstring : int -> Internal_buffer.bs t
  val create_by        : proof:'a Internal_buffer.t -> int -> 'a t
  val proof            : 'a t -> 'a Internal_buffer.t
  val size             : 'a t -> int
  val ravailable       : 'a t -> int
  val wavailable       : 'a t -> int
  val radvance         : 'a t -> int -> unit
  val wadvance         : 'a t -> int -> unit
  val peek             : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
  val read             : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
  val get              : 'a t -> char
  val write            : 'a t -> 'a Internal_buffer.t -> int -> int -> unit
  val write_string     : 'a t -> string -> int -> int -> unit

  val read_space       : 'a t -> ('a Internal_buffer.t * int * int) option
  val write_space      : 'a t -> ('a Internal_buffer.t * int * int) option
  val transmit         : 'a t -> ('a Internal_buffer.t -> int -> int -> int) -> int
  val pp               : Format.formatter -> 'a t -> unit
  val rollback         : 'a t -> 'a Internal_buffer.t -> unit
end

module type C =
sig
  include S

  type mark

  val proof       : 'a t -> 'a Internal_buffer.t
  val mark        : 'a t -> mark
  val unmark      : mark -> 'a t -> unit
  val forget      : mark -> 'a t -> unit
  val equal       : mark -> mark -> bool
  val savailable  : 'a t -> int
end

type 'a t =
  { size         : int
  ; buffer       : 'a Internal_buffer.t
  ; mutable rpos : int
  ; mutable wpos : int }

let pp fmt { rpos; wpos; buffer; size; } =
  if rpos <= wpos
  then Format.fprintf fmt "{ @[<hov>size = %d;@ %d.@,%a@ [ rpos ]@ %a@ [ wpos ]@ %a@,.%d@] }" (size - 1)
         rpos
         Internal_buffer.pp (Internal_buffer.sub buffer 0 rpos)
         Internal_buffer.pp (Internal_buffer.sub buffer rpos (wpos - rpos))
         Internal_buffer.pp (Internal_buffer.sub buffer wpos (size - wpos))
         wpos
  else Format.fprintf fmt "{ @[<hov>size = %d;@ %d.@,%a@ [ wpos ]@ %a@ [ rpos ]@ %a@,.%d@] }" (size - 1)
         wpos
         Internal_buffer.pp (Internal_buffer.sub buffer 0 wpos)
         Internal_buffer.pp (Internal_buffer.sub buffer wpos (rpos - wpos))
         Internal_buffer.pp (Internal_buffer.sub buffer rpos (size - rpos))
         rpos

let proof (type a) (v : a t) : a Internal_buffer.t =
  v.buffer

let size { size; _ } = size - 1

let create_bytes size =
  { size   = size + 1
  ; buffer = Internal_buffer.create_bytes (size + 1)
  ; rpos   = 0
  ; wpos   = 0 }

let create_bigstring size =
  { size   = size + 1
  ; buffer = Internal_buffer.create_bigstring (size + 1)
  ; rpos   = 0
  ; wpos   = 0 }

let create_by ~proof size =
  { size   = size + 1
  ; buffer = Internal_buffer.create_by ~proof (size + 1)
  ; rpos   = 0
  ; wpos   = 0 }

let ravailable t =
  if t.wpos >= t.rpos then (t.wpos - t.rpos)
  else t.size - (t.rpos - t.wpos)

let wavailable t =
  if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
  else (t.rpos - t.wpos) - 1

let radvance t n =
  assert (n <= ravailable t);

  if t.rpos + n < t.size then t.rpos <- t.rpos + n
  else t.rpos <- t.rpos + n - t.size

let wadvance t n =
  assert (n <= wavailable t);

  if t.wpos + n < t.size then t.wpos <- t.wpos + n
  else t.wpos <- t.wpos + n - t.size

let peek t buff off len =
  assert (len <= ravailable t);

  let pre = t.size - t.rpos in
  let extra = len - pre in
  if extra > 0 then begin
    Internal_buffer.blit t.buffer t.rpos buff off pre;
    Internal_buffer.blit t.buffer 0 buff (off + pre) extra;
  end else
    Internal_buffer.blit t.buffer t.rpos buff off len

let read t buff off len =
  peek t buff off len;
  radvance t len

let get (type a) (t : a t) =
  (* TODO: optimize *)

  let tmp = Internal_buffer.create_by ~proof:t.buffer 1 in
  peek t tmp 0 1;
  Internal_buffer.get tmp 0

let write t buff off len =
  assert (len <= wavailable t);

  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then begin
    Internal_buffer.blit buff off t.buffer t.wpos pre;
    Internal_buffer.blit buff (off + pre) t.buffer 0 extra;
  end else
    Internal_buffer.blit buff off t.buffer t.wpos len;

  wadvance t len

let write_string t buff off len =
  assert (len <= wavailable t);

  let pre = t.size - t.wpos in
  let extra = len - pre in
  if extra > 0 then begin
    Internal_buffer.blit_string buff off t.buffer t.wpos pre;
    Internal_buffer.blit_string buff (off + pre) t.buffer 0 extra;
  end else
    Internal_buffer.blit_string buff off t.buffer t.wpos len;

  wadvance t len

let read_space t =
  if t.wpos = t.rpos then None
  else let len0 =
         if t.wpos >= t.rpos then t.wpos - t.rpos
         else t.size - t.rpos
       in

       Some (t.buffer, t.rpos, len0)

let write_space t =
  if wavailable t = 0 then None
  else let len0 =
         if t.wpos >= t.rpos
         then (if t.rpos = 0 then t.size - t.wpos - 1 else t.size - t.wpos)
              (* XXX: wavailable asserts than if t.rpos = 0,
               *      t.wpos <> t.size - 1 *)
         else (t.rpos - t.wpos - 1)
       in

       Some (t.buffer, t.wpos, len0)

let transmit t f =
  if t.wpos = t.rpos then 0
  else let len0 =
         if t.wpos >= t.rpos then t.wpos - t.rpos
         else t.size - t.rpos
       in
       let len = f t.buffer t.rpos len0 in
       assert (len <= len0);
       radvance t len;
       len

let rollback t s =
  assert (Internal_buffer.length s <= wavailable t);

  if t.rpos <= t.wpos
  then let pre = min t.rpos (Internal_buffer.length s) in
       let extra = (Internal_buffer.length s) - pre in

       if extra > 0
       then begin
         Internal_buffer.blit s 0 t.buffer (t.size - extra) extra;
         Internal_buffer.blit s extra t.buffer 0 pre;

         t.rpos <- t.size - extra;
       end else begin
         Internal_buffer.blit s 0 t.buffer (t.rpos - pre) pre;

         t.rpos <- t.rpos - pre;
       end
  else begin
    Internal_buffer.blit s 0 t.buffer (t.rpos - (Internal_buffer.length s)) (Internal_buffer.length s);

    t.rpos <- t.rpos - (Internal_buffer.length s)
  end

module Ext =
struct
  type nonrec 'a t = { mutable contents : 'a t }

  let prepare t len =
    if wavailable t.contents > len
    then t.contents
    else let buffer = create_by ~proof:t.contents.buffer (ravailable t.contents + len) in
         while ravailable t.contents <> 0
         do ignore (transmit t.contents (fun buff off len -> write buffer buff off len; len)) done;
         t.contents <- buffer;
         buffer

  let write t buff off len =
    let buffer = prepare t len in
    write buffer buff off len

  let write_string t buff off len =
    let buffer = prepare t len in
    write_string buffer buff off len

  let create_bytes len =
    { contents = create_bytes len }
  let create_bigstring len =
    { contents = create_bigstring len }
  let create_by ~proof len =
    { contents = create_by ~proof len }

  let size t           = size t.contents
  let peek t           = peek t.contents
  let read t           = read t.contents
  let read_space t     = read_space t.contents
  let write_space t    = write_space t.contents
  let transmit t       = transmit t.contents
  let ravailable t     = ravailable t.contents
  let wavailable t     = wavailable t.contents
  let radvance t       = radvance t.contents
  let wadvance t       = wadvance t.contents
  let get t            = get t.contents
  let pp fmt t         = pp fmt t.contents
  let rollback t s     = rollback t.contents s
  let proof t          = proof t.contents
end

module Committed =
struct
  type nonrec 'a t =
    { mutable contents : 'a t
    ; mutable mark : int option }

  type mark =
    | Weak of int
    | Uniq of int

  let copy t len =
    let buffer = create_by ~proof:t.contents.buffer (ravailable t.contents + len) in
    while ravailable t.contents <> 0
    do ignore (transmit t.contents (fun buff off len -> write buffer buff off len; len)) done;

    t.contents <- buffer;
    buffer

  let prepare t len =
    let kcopy mark t len =
      assert (t.contents.rpos <= t.contents.wpos);

      let buffer = create_by ~proof:t.contents.buffer (t.contents.wpos + len) in
      Internal_buffer.blit t.contents.buffer mark buffer.buffer mark (t.contents.wpos - mark);
      buffer.rpos <- t.contents.rpos; (* XXX: we keep rpos *)
      buffer.wpos <- t.contents.wpos;

      t.contents <- buffer;
      buffer
    in

    match t.mark with
    | None ->
      if wavailable t.contents >= len
      then t.contents
      else copy t len
    | Some mark ->
      match write_space t.contents with
      (* enough continuous space *)
      | Some (_, _, len') when len' > len -> t.contents
      (* not enough continuous space *)
      | Some _ | None -> kcopy mark t len

  let equal a b = match a, b with
    | Weak a, Weak b -> a = b
    | Weak a, Uniq b
    | Uniq b, Weak a -> a = b
    | Uniq a, Uniq b -> a = b

  let mark t = match t.mark with
    | Some mark -> Weak t.contents.rpos
    | None ->
      let buffer = create_by ~proof:t.contents.buffer (size t.contents) in
      while ravailable t.contents <> 0
      do ignore (transmit t.contents (fun buff off len -> write buffer buff off len; len)) done;

      t.contents <- buffer; (* XXX: we assert than rpos <= wpos *)
      t.mark <- Some t.contents.rpos; (* XXX: we store rpos *)
      assert (t.contents.rpos = 0);
      Uniq t.contents.rpos

  let unmark mark t = match mark, t.mark with
    | _, None -> raise (Invalid_argument "unmark")
    | Weak rpos, Some mark ->
      t.contents.rpos <- rpos
    | Uniq rpos, Some mark ->
      assert (mark = rpos);
      t.contents.rpos <- rpos;
      t.mark <- None

  let forget mark t = match mark, t.mark with
    | _, None -> raise (Invalid_argument "forget")
    | Weak _, _ -> ()
    | Uniq _, _ ->
      t.mark <- None

  let write t buff off len =
    let r = prepare t len in
    write r buff off len

  let write_string t buff off len =
    let r = prepare t len in
    write_string r buff off len

  let create_bytes len =
    { contents = create_bytes len
    ; mark = None }
  let create_bigstring len =
    { contents = create_bigstring len
    ; mark = None }
  let create_by ~proof len =
    { contents = create_by ~proof len
    ; mark = None }

  let size t           = size t.contents
  let peek t           = peek t.contents
  let read t           = read t.contents
  let read_space t     = read_space t.contents
  let write_space t    = write_space t.contents
  let transmit t       = transmit t.contents
  let ravailable t     = ravailable t.contents
  let wavailable t     = wavailable t.contents
  let radvance t       = radvance t.contents
  let wadvance t       = wadvance t.contents
  let get t            = get t.contents
  let pp fmt t         = pp fmt t.contents
  let rollback t s     = rollback t.contents s
  let proof t          = proof t.contents

  let savailable ({ mark; _ } as t) =
    match mark with
    | None -> ravailable t
    | Some mark -> t.contents.wpos - mark
end
