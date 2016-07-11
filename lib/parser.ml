let () = Printexc.record_backtrace true

let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

module type S =
sig
  type s =
    | Complete
    | Incomplete

  val pp : Format.formatter -> s -> unit

  type err = ..

  type ('a, 'input) state =
    | Read of { buffer : 'input Input.t; k : int -> s -> ('a, 'input) state }
    | Done of 'a
    | Fail of string list * err

  type ('a, 'input) k    = 'input Input.t -> s -> 'a
  type ('a, 'input) fail = (string list -> err -> ('a, 'input) state, 'input) k
  type ('a, 'r, 'input) success = ('a -> ('r, 'input) state, 'input) k

  type 'a t =
    { f : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k }

  val return : 'a -> 'a t
  val fail   : err -> 'a t

  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|)  : 'a t -> ('a -> 'b) -> 'b t
  val (<|>)  : 'a t -> 'a t -> 'a t
  val (<$>)  : ('a -> 'b) -> 'a t -> 'b t
  val (<* )  : 'a t -> 'b t -> 'a t
  val ( *>)  : 'a t -> 'b t -> 'b t
  val (<*>)  : ('a -> 'b) t -> 'a t -> 'b t
  val (<?>)  : 'a t -> string -> 'a t
  val fix    : ('a t -> 'a t) -> 'a t
  val lift   : ('a -> 'b) -> 'a t -> 'b t
  val lift2  : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3  : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val run    : 'input Input.t -> 'a t -> ('a, 'input) state
  val only   : 'input Input.t -> 'a t -> ('a, 'input) state
end

type s =
  | Complete
  | Incomplete

let pp fmt = function
  | Complete -> Format.pp_print_string fmt "Complete"
  | Incomplete -> Format.pp_print_string fmt "Incomplete"

type err = ..

type ('a, 'input) state =
  | Read of { buffer : 'input Input.t; k : int -> s -> ('a, 'input) state }
  | Done of 'a
  | Fail of string list * err

type ('a, 'input) k           = 'input Input.t -> s -> 'a
type ('a, 'input) fail        = (string list -> err -> ('a, 'input) state, 'input) k
type ('a, 'r, 'input) success = ('a -> ('r, 'input) state, 'input) k

type 'a t =
  { f : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k }

let return v = { f = fun i s _ succ -> succ i s v }
let fail err = { f = fun i s fail _ -> fail i s [] err }

let (>>=) a f = { f = fun i s fail succ ->
  let succ' i' s' v = (f v).f i' s' fail succ in
  a.f i s fail succ' }

let (>>|) a f = { f = fun i s fail succ ->
  let succ' i' s' v = succ i' s' (f v) in
  a.f i s fail succ' }

let (<$>) f m = m >>| f

let lift f m = f <$> m

let (<|>) u v =
  { f = fun i s fail succ ->
    let mark = Input.mark i in

    let fail' i s marks err =
      Input.unmark mark i; (* we rollback to old read position *)
      v.f i s fail succ
    in

    let succ' i s e =
      if Input.equal (Input.mark i) mark
      then begin
        Input.unmark mark i;
        v.f i s fail succ
      end else begin
        Input.forget mark i; (* forget the mark, and may be switch to
                                ring-buffer *)
        succ i s e
      end
    in

    u.f i s fail' succ' }

let (<* ) a b =
  { f = fun i s fail succ ->
    let succ' i' s' x =
      let succ'' i'' s'' _ = succ i'' s'' x in
      b.f i' s' fail succ'' in
    a.f i s fail succ' }

let ( *>) a b =
  { f = fun i s fail succ ->
    let succ' i' s' _ = b.f i' s' fail succ in
    a.f i s fail succ' }

let (<*>) u v =
  { f = fun i s fail succ ->
    let succ' i' s' f =
      let succ'' i'' s'' m =
        succ i'' s'' (f m)
      in
      v.f i' s' fail succ''
    in
    u.f i s fail succ' }

let (<?>) a mark =
  { f = fun i s fail succ ->
    let fail' i' s' marks err =
      fail i' s' (mark :: marks) err in
    a.f i s fail' succ }

let lift2 f m1 m2 =
  { f = fun i s fail succ ->
    let succ' i' s' m' =
      let succ'' i'' s'' m'' = succ i'' s'' (f m' m'') in
      m2.f i' s' fail succ''
    in
    m1.f i s fail succ' }

let lift3 f m1 m2 m3 =
  { f = fun i s fail succ ->
    let succ' i' s' m' =
      let succ'' i'' s'' m'' =
        let succ''' i''' s''' m''' = succ i''' s''' (f m' m'' m''') in
        m3.f i'' s'' fail succ'''
      in
      m2.f i' s' fail succ''
    in
    m1.f i s fail succ' }

let ignore p = p *> return ()

let fix f =
  let rec u = lazy (f r)
  and r = { f = fun i s fail succ ->
            Lazy.(force u).f i s fail succ }
  in r

let run buffer a =
  let fail' buf _ marks err = Fail (marks, err) in
  let succeed' buf _ value  = Done value in
  a.f buffer Incomplete fail' succeed'

let only buffer a =
  let fail' buf _ marks err = Fail (marks, err) in
  let succeed' buf _ value = Done value in
  a.f buffer Complete fail' succeed'

module type I =
sig
  type err += End_of_flow

  val prompt  : 'input Input.t -> ('input Input.t -> s -> ('a, 'input) state) -> ('input Input.t -> s -> ('a, 'input) state) -> ('a, 'input) state
  val expect  : unit t
  val require : int -> 'input Input.t -> s -> ('a, 'input) fail -> (unit, 'a, 'input) success -> ('a, 'input) state
  val ensure  : int -> string t
end

module type C =
sig
  type err += Satisfy
  type err += String
  type err += Repeat

  val peek_chr     : char option t
  val peek_chr_exn : char t
  val advance      : int -> unit t
  val satisfy      : (char -> bool) -> char t
  val print        : string -> unit t
  val string       : (string -> string) -> string -> string t
  val store        : Buffer.t -> (char -> bool) -> int t
  val recognize    : (char -> bool) -> string t
  val char         : char -> char t
  val many         : 'a t -> 'a list t
  val one          : 'a t -> 'a list t
  val option       : 'a -> 'a t -> 'a t
  val take         : int -> string t
  val list         : 'a t list -> 'a list t
  val count        : int -> 'a t -> 'a list t
  val repeat'      : Buffer.t -> int option -> int option -> (char -> bool) -> int t
  val repeat       : int option -> int option -> (char -> bool) -> string t
end

module IO : I =
struct
  type err += End_of_flow

  let rec prompt i fail succ =
    let continue n s =
      if n = 0 then
        if s = Complete
        then fail i Complete
        else prompt i fail succ
      else succ i s
    in

    Read { buffer = i; k = continue; }

  let expect =
    { f = fun i s fail succ ->
      match s with
      | Complete -> fail i s [] End_of_flow
      | Incomplete ->
        let succ' i' s' = succ i' s' () in
        let fail' i' s' = fail i' s' [] End_of_flow in
        prompt i fail' succ' }

  let require n i s fail succ =
    let rec continue = { f = fun i' s' fail' succ' ->
      if n < Input.ravailable i'
      then succ' i' s' ()
      else (expect >>= fun () -> continue).f i' s' fail' succ' }
    in

    (expect >>= fun () -> continue).f i s fail succ

  let ensure n =
    let sub n =
      let f
        : 'r 'input. (('r, 'input) fail -> ('a, 'r, 'input) success -> ('r, 'input) state, 'input) k
        = fun i s fail succ ->
          let tmp = Internal_buffer.create_by ~proof:(Input.proof i) n in
          Input.peek i tmp 0 n;
          succ i s (Internal_buffer.sub_string tmp 0 (Internal_buffer.length tmp))
      in
      { f }
    in
    { f = fun i s fail succ ->
      if Input.ravailable i >= n
      then succ i s ()
      else require n i s fail succ }
    >>= fun () -> sub n
end

module Convenience : C =
struct
  let peek_chr = { f = fun i s fail succ ->
    if Input.ravailable i > 0
    then succ i s (Some (Input.get i))
    else if s = Complete
    then succ i s None
    else
      let succ' i' s' =
        succ i' s' (Some (Input.get i')) in
      let fail' i' s' =
        succ i' s' None in
      IO.prompt i fail' succ' }

  let peek_chr_exn = { f = fun i s fail succ ->
    if Input.ravailable i > 0
    then succ i s (Input.get i)
    else let succ' i' s' () =
           succ i' s' (Input.get i') in
         IO.require 1 i s fail succ' }

  let advance n =
    { f = fun i s fail succ -> Input.radvance i n; succ i s () }

  type err += Satisfy

  let satisfy f =
    peek_chr_exn >>= fun chr ->
      if f chr
      then advance 1 >>| fun () -> chr
      else fail Satisfy

  type err += String

  let sp = Format.sprintf

  let print t =
    { f = fun i s fail succ ->
      Printf.printf "%s%!" t;
      succ i s () }

  let string f s =
    let len = String.length s in
    IO.ensure len >>= fun s' ->
      if f s = f s'
      then advance len *> return s'
      else fail String

  let store buffer f =
    { f = fun i s fail succ ->
      let recognize buff off len =
        let len' = locate buff off len f in
        Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
        len'
      in

      let consumed = Input.transmit i recognize in

      succ i s consumed }

  let recognize f =
    { f = fun i s fail succ ->
      let buffer = Buffer.create 16 in

      let r = fix @@ fun m ->
        peek_chr >>= function
        | Some chr when f chr -> store buffer f >>= fun _ -> m
        | Some _ | None -> return (Buffer.length buffer)
      in

      let succ' i s consumed = succ i s (Buffer.contents buffer) in

      r.f i s fail succ' }

  let char chr =
    satisfy ((=) chr) <?> (String.make 1 chr)

  let many p =
    fix (fun m -> (lift2 (fun x r -> x :: r) p m) <|> return [])

  let one p =
    lift2 (fun x r -> x :: r) p (many p)

  let option x p =
    p <|> return x

  let take n =
    let n = max n 0 in
    IO.ensure n >>= fun str ->
    advance n >>| fun () -> str

  let rec list l =
    match l with
    | [] -> return []
    | x :: r -> lift2 (fun x r -> x :: r) x (list r)

  let count n p =
    assert (n >= 0);

    let rec loop = function
      | 0 -> return []
      | n -> lift2 (fun x r -> x :: r) p (loop (n - 1))
    in

    loop n

  type err += Repeat

  let repeat' buffer a b f =
    let at_least n = match a with
      | Some a -> n >= a
      | None -> true
    in
    let is_most n = match b with
      | Some b -> n = b
      | None -> false
    in
    fix @@ fun m ->
    peek_chr >>= function
    | Some chr when f chr && not (is_most (Buffer.length buffer)) ->
      { f = fun i s fail succ ->
        let consumed = Input.transmit i
          (fun buff off len ->
           let len' = locate buff off len f in

           let len' = match b with
             | Some b when (Buffer.length buffer + len') > b ->
               b - (Buffer.length buffer)
             | _ -> len'
           in
           Buffer.add_string buffer (Internal_buffer.sub_string buff off len');
           len') in

        succ i s consumed } *> m
    | _ ->
      if at_least (Buffer.length buffer)
      then return (Buffer.length buffer)
      else fail Repeat

  let repeat a b f =
    { f = fun i s fail succ ->
      let buffer = Buffer.create 16 in

      let succ' i s n = succ i s (Buffer.contents buffer) in

      (repeat' buffer a b f).f i s fail succ' }
end
