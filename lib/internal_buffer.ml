open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

type bs = [ `BS ]
type st = [ `ST ]

module Bigstring =
struct
  open Bigarray

  type t = bigstring

  let length = Array1.dim
  let create = Array1.create Bigarray.Char c_layout

  let get : t -> int -> char         = Array1.get
  let set : t -> int -> char -> unit = Array1.set
  let sub : t -> int -> int -> t     = Array1.sub
  let fill : t -> char -> unit       = Array1.fill

  let to_string v =
    let buf = Bytes.create (length v) in
    for i = 0 to length v - 1
    do Bytes.set buf i (get v i) done;
    Bytes.unsafe_to_string buf

  let sub_string t off len =
    let r = Bytes.create len in
    let s = sub t off len in
    Bytes.iteri (fun i _ -> Bytes.set r i (get s i)) r;
    Bytes.unsafe_to_string r

  let blit src src_idx dst dst_idx len =
    let src = Array1.sub src src_idx len in
    let dst = Array1.sub dst dst_idx len in
    Array1.blit src dst

  let blit_string src src_idx dst dst_idx len =
    let idx = ref 0 in

    while !idx < len do
      Array1.set dst (dst_idx + !idx) (String.get src (src_idx + !idx));
      incr idx;
    done

  let blit_bytes src src_idx dst dst_idx len =
    let idx = ref 0 in

    while !idx < len do
      Bytes.set dst (dst_idx + !idx) (Array1.get src (src_idx + !idx));
      incr idx;
    done

  let pp fmt bs =
    for i = 0 to length bs - 1
    do match get bs i with
       | '\000' .. '\031' | '\127' -> Format.pp_print_char fmt '.'
       | chr -> Format.pp_print_char fmt chr
    done;
end

type 'a t =
  | Bytes     : bytes -> st t
  | Bigstring : bigstring -> bs t

let from_bytes v     = Bytes v
let from_bigstring v = Bigstring v

let from_string (type a) ~(proof:a t) s : a t = match proof with
  | Bytes _ -> Bytes (Bytes.of_string s)
  | Bigstring _ ->
    let l = String.length s in
    let b = Bigstring.create l in
    Bigstring.blit_string s 0 b 0 l;
    Bigstring b

let create_bytes size =
  Bytes (Bytes.create size)

let create_bigstring size =
  Bigstring (Bigstring.create size)

let create_by (type a) ~(proof:a t) size : a t =
  match proof with
  | Bytes _ -> create_bytes size
  | Bigstring _ -> create_bigstring size

let length (type a) (v : a t) = match v with
  | Bytes v -> Bytes.length v
  | Bigstring v -> Bigstring.length v

let get (type a) (v : a t) = match v with
  | Bytes v -> Bytes.get v
  | Bigstring v -> Bigstring.get v

let set (type a) (v : a t) = match v with
  | Bytes v -> Bytes.set v
  | Bigstring v -> Bigstring.set v

let blit (type a) (src : a t) src_idx (dst : a t) dst_idx len =
  match src, dst with
  | Bytes src, Bytes dst ->
    Bytes.blit src src_idx dst dst_idx len
  | Bigstring src, Bigstring dst ->
    Bigstring.blit src src_idx dst dst_idx len

let blit_string (type a) src src_idx (dst : a t) dst_idx len =
  match dst with
  | Bytes dst -> Bytes.blit_string src src_idx dst dst_idx len
  | Bigstring dst -> Bigstring.blit_string src src_idx dst dst_idx len

let blit_bytes (type a) (src : a t) src_idx dst dst_idx len =
  match src with
  | Bytes src -> Bytes.blit src src_idx dst dst_idx len
  | Bigstring src -> Bigstring.blit_bytes src src_idx dst dst_idx len

let sub (type a) (v : a t) off len : a t = match v with
  | Bytes v -> from_bytes @@ Bytes.sub v off len
  | Bigstring v -> from_bigstring @@ Bigstring.sub v off len

let sub_string (type a) (v : a t) off len = match v with
  | Bytes v -> Bytes.sub_string v off len
  | Bigstring v -> Bigstring.sub_string v off len

let to_string (type a) (v : a t) = match v with
  | Bytes v -> Bytes.to_string v
  | Bigstring v -> Bigstring.to_string v

let pp_bytes fmt =
  Bytes.iter @@ function
    | '\000' .. '\031' | '\127' -> Format.pp_print_char fmt '.'
    | chr -> Format.pp_print_char fmt chr

let pp (type a) fmt (v : a t) = match v with
  | Bytes v -> pp_bytes fmt v
  | Bigstring v -> Bigstring.pp fmt v
