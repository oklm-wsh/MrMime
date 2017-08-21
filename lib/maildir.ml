module Map = Map.Make(String)

type safe
type unsafe

type 'a t =
  { sequence      : int option
  ; boot          : int option
  ; crypto_random : int option
  ; inode         : int option
  ; device        : int option
  ; microsecond   : int option
  ; pid           : int option
  ; deliveries    : int option }

let make ?sequence
         ?boot
         ?crypto_random
         ?inode
         ?device
         ?microsecond
         ?pid
         ?deliveries () : unsafe t =
  { sequence
  ; boot
  ; crypto_random
  ; inode
  ; device
  ; microsecond
  ; pid
  ; deliveries }

let to_safe (x : unsafe t) : safe t option =
  let open Option in
  if is_some x.sequence
     || is_some x.boot
     || is_some x.crypto_random
     || is_some x.inode
     || is_some x.device
     || is_some x.microsecond
     || is_some x.pid
     || is_some x.deliveries
  then Some { sequence      = x.sequence
            ; boot          = x.boot
            ; crypto_random = x.crypto_random
            ; inode         = x.inode
            ; device        = x.device
            ; microsecond   = x.microsecond
            ; pid           = x.pid
            ; deliveries    = x.deliveries }
  else None

type id =
  | Modern of safe t
  | Old0 of int
  | Old1 of int * int

type flag =
  | Passed
  | Replied
  | Seen
  | Trashed
  | Draft
  | Flagged

type 'a info =
  | Exp of 'a
  | Info of flag list

type 'a filename =
  { time                 : int
  ; id                   : id
  ; host                 : string
  ; parameters           : string Map.t
  ; info                 : 'a info }

let pp = Format.fprintf

let pp_lst ~sep pp_data fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp fmt "%a%a" pp_data x sep (); aux r
  in aux lst

let pp_flag fmt = function
  | Passed  -> pp fmt "Passed"
  | Replied -> pp fmt "Replied"
  | Seen    -> pp fmt "Seen"
  | Trashed -> pp fmt "Trashed"
  | Draft   -> pp fmt "Draft"
  | Flagged -> pp fmt "Flagged"

let pp_info pp_experimental fmt = function
  | Exp a -> pp_experimental fmt a
  | Info l ->
    pp fmt "[@[<hov>%a@]]"
      (pp_lst ~sep:(fun fmt () -> pp fmt ";@ ") pp_flag) l

let pp_option pp_data fmt = function
  | Some a -> pp_data fmt a
  | None -> pp fmt "<none>"

let pp_t fmt { sequence; boot; crypto_random; inode; device; microsecond; pid; deliveries; } =
  pp fmt "{@[<hov>sequence = %a;@ \
                  boot = %a;@ \
                  crypto_random = %a;@ \
                  inode = %a;@ \
                  device = %a;@ \
                  microsecond = %a;@ \
                  pid = %a;@ \
                  deliveries = %a@]}"
    (pp_option Format.pp_print_int) sequence
    (pp_option Format.pp_print_int) boot
    (pp_option Format.pp_print_int) crypto_random
    (pp_option Format.pp_print_int) inode
    (pp_option Format.pp_print_int) device
    (pp_option Format.pp_print_int) microsecond
    (pp_option Format.pp_print_int) pid
    (pp_option Format.pp_print_int) deliveries

let pp_id fmt = function
  | Modern t    -> pp fmt "Modern %a" pp_t t
  | Old0 i      -> pp fmt "Old %d" i
  | Old1 (a, b) -> pp fmt "Old (%d, %d)" a b

let pp_map fmt map =
  Map.iter
    (fun key value -> pp fmt "%s -> %s@\n" key value)
    map

let pp_filename pp_experimental fmt { time; id; host; parameters; info; } =
  pp fmt "{@[<hov>time = %d;@ \
                  id = @[<hov>%a@];@ \
                  host = %S;@ \
                  parameters = @[<hov>%a@];@ \
                  info = @[<hov]%a@]@]}"
    time
    pp_id id
    host
    pp_map parameters
    (pp_info pp_experimental) info

open Parser
open Parser.Convenience

type err += Invalid_filename

let implode l =
  let s = Bytes.create (List.length l) in
  let rec aux i = function
    | [] -> Bytes.unsafe_to_string s
    | x :: r -> Bytes.set s i x; aux (i + 1) r
  in
  aux 0 l

let avoid = return ()

let parse experimental =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let digit    = repeat (Some 1) None is_digit >>| int_of_string in

  let sequence      = char '#' *> digit in
  let boot          = char 'X' *> digit in
  let crypto_random = char 'R' *> digit in
  let inode         = char 'I' *> digit in
  let device        = char 'V' *> digit in
  let microsecond   = char 'M' *> digit in
  let pid           = char 'P' *> digit in
  let deliveries    = char 'Q' *> digit in

  let modern =
        (sequence >>| fun x -> `Sequence x)
    <|> (boot >>| fun x -> `Boot x)
    <|> (crypto_random >>| fun x -> `CryptoRandom x)
    <|> (inode >>| fun x -> `Inode x)
    <|> (device >>| fun x -> `Device x)
    <|> (microsecond >>| fun x -> `Microsecond x)
    <|> (pid >>| fun x -> `Pid x)
    <|> (deliveries >>| fun x -> `Deliveries x)
  in

  let modern =
    one modern
    >>= fun l ->
      { f = fun i s _fail succ ->

        let rec catch acc = function
        | `Sequence x :: r     -> catch { acc with sequence = Some x } r
        | `Boot x :: r         -> catch { acc with boot = Some x } r
        | `CryptoRandom x :: r -> catch { acc with crypto_random = Some x } r
        | `Inode x :: r        -> catch { acc with inode = Some x } r
        | `Device x :: r       -> catch { acc with device = Some x } r
        | `Microsecond x :: r  -> catch { acc with microsecond = Some x } r
        | `Pid x :: r          -> catch { acc with pid = Some x } r
        | `Deliveries x :: r   -> catch { acc with deliveries = Some x } r
        | [] -> acc in

        succ i s (catch (make ()) l) }
    >>| to_safe
  in

  let host =
    one
      (    (string (fun x -> x) "\057" >>= fun _ -> return '/')
       <|> (string (fun x -> x) "\072" >>= fun _ -> return ':')
       <|> (satisfy (function ',' -> false | _ -> true)))
    >>| implode
  in

  let parameters =
    one (   repeat None None (function '=' | ':' | ',' -> false | _ -> true)
         >>= fun k -> char '='
         *> repeat None None (function '=' | ':' | ',' -> false | _ -> true)
         >>= fun v -> return (k, v))
    >>| fun lst -> List.fold_right (fun (key, value) -> Map.add key value) lst Map.empty
  in

  digit
  >>= fun time       -> (    (modern >>= function Some x -> return (Modern x)
                                          | None -> fail Invalid_filename)
                         <|> (digit <* char '_' >>= fun n -> digit >>| fun m -> Old1 (n, m))
                         <|> (digit >>| fun x -> Old0 x))
  >>= fun id         -> host
  >>= fun host       -> option Map.empty (char ',' *> parameters)
  >>= fun parameters -> char ':' *> peek_chr >>= function
    | Some '1' -> char ',' *> experimental >>| fun e ->
                  { time; id; host; parameters; info = Exp e }
    | Some '2' ->
      let flag =
            (char 'P' >>| fun _ -> Passed)
        <|> (char 'R' >>| fun _ -> Replied)
        <|> (char 'S' >>| fun _ -> Seen)
        <|> (char 'T' >>| fun _ -> Trashed)
        <|> (char 'D' >>| fun _ -> Draft)
        <|> (char 'F' >>| fun _ -> Flagged)
      in

      let flags =
        fix @@ fun m -> (lift2 (function Some x -> fun r -> x :: r
                                       | None -> fun r -> r)
                               (option None (flag >>| fun x -> Some x)) m)
                        <|> return []
      in

      char ',' *> flags >>| fun l ->
      { time; id; host; parameters; info = Info l }
    | _ -> return { time; id; host; parameters; info = Info [] }

let of_filename experimental filename =
  let l = String.length filename in
  let i = Input.create_bytes 128 in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min 128 (l - consumed) in
      Input.write_string buffer filename consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  aux 0 @@ Parser.run i (parse experimental)
