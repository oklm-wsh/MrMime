type day   = Rfc5322.day =
  | Mon | Tue | Wed
  | Thu | Fri | Sat
  | Sun
type month = Rfc5322.month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec
type zone  = Rfc5322.zone =
  | UT  | GMT
  | EST | EDT
  | CST | CDT
  | MST | MDT
  | PST | PDT
  | Military_zone of char
  | TZ of int

type date = Rfc5322.date =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; zone : zone }

(* convenience alias *)
module Input = MrMime_input

let pp = Format.fprintf

let pp_zone fmt = function
  | UT   -> pp fmt "UT"
  | GMT  -> pp fmt "GMT"
  | EST  -> pp fmt "EST"
  | EDT  -> pp fmt "EDT"
  | CST  -> pp fmt "CST"
  | CDT  -> pp fmt "CDT"
  | MST  -> pp fmt "MST"
  | MDT  -> pp fmt "MDT"
  | PST  -> pp fmt "PST"
  | PDT  -> pp fmt "PDT"
  | TZ z -> pp fmt "(TZ %04d)" z
  | Military_zone c -> pp fmt "(Military_zone %c)" c

let pp_month fmt = function
  | Jan  -> pp fmt "Jan"
  | Feb  -> pp fmt "Feb"
  | Mar  -> pp fmt "Mar"
  | Apr  -> pp fmt "Apr"
  | May  -> pp fmt "May"
  | Jun  -> pp fmt "Jun"
  | Jul  -> pp fmt "Jul"
  | Aug  -> pp fmt "Aug"
  | Sep  -> pp fmt "Sep"
  | Oct  -> pp fmt "Oct"
  | Nov  -> pp fmt "Nov"
  | Dec  -> pp fmt "Dec"

let pp_day fmt = function
  | Mon  -> pp fmt "Mon"
  | Tue  -> pp fmt "Tue"
  | Wed  -> pp fmt "Wed"
  | Thu  -> pp fmt "Thu"
  | Fri  -> pp fmt "Fri"
  | Sat  -> pp fmt "Sat"
  | Sun  -> pp fmt "Sun"

let pp fmt = function
  | { day = Some day; date = (d, m, y); time = (hh, mm, ss); zone; } ->
    pp fmt "{@[<hov>day = %a;@ \
                    date = (@[<hov>%d,@ %a,@ %d@]);@ \
                    time = (@[<hov>%d,@ %d,@ %d@]);@ \
                    zone = %a@]}"
      pp_day day d pp_month m y hh mm (Option.value ~default:0 ss)
      pp_zone zone
  | { day = None; date = (d, m, y); time = (hh, mm, ss); zone; } ->
    pp fmt "{@[<hov>date = (@[<hov>%d,@ %a,@ %d@]);@ \
                    time = (@[<hov>%d,@ %d,@ %d@]);@ \
                    zone = %a@]}"
      d pp_month m y hh mm (Option.value ~default:0 ss)
      pp_zone zone

module Encoder =
struct
  include Encoder
  open Wrap

  let w_day = function
    | Sat -> string "Sat"
    | Fri -> string "Fri"
    | Mon -> string "Mon"
    | Wed -> string "Wed"
    | Sun -> string "Sun"
    | Tue -> string "Tue"
    | Thu -> string "Thu"

  let w_date (day, month, year) =
    hovbox 0
    $ string (string_of_int day)
    $ close_box
    $ space
    $ hovbox 0
    $ (match month with
       | Feb -> string "Feb"
       | Mar -> string "Mar"
       | Dec -> string "Dec"
       | Jul -> string "Jul"
       | Sep -> string "Sep"
       | Nov -> string "Nov"
       | Aug -> string "Aug"
       | Jun -> string "Jun"
       | May -> string "May"
       | Apr -> string "Apr"
       | Oct -> string "Oct"
       | Jan -> string "Jan")
    $ close_box
    $ space
    $ hovbox 0
    $ string (string_of_int year)
    $ close_box

  let w_time (hh, mm, ss) =
    hovbox 0
    $ string (sp "%02d" hh)
    $ close_box
    $ string ":"
    $ hovbox 0
    $ string (sp "%02d" mm)
    $ close_box
    $ match ss with
      | Some ss -> string ":" $ hovbox 0 $ string (sp "%02d" ss) $ close_box
      | None -> noop

  let w_zone = function
  | TZ zone -> string (sp "%c%04d" (if zone < 0 then '-' else '+') (abs zone))
  | Military_zone c -> string (sp "%c" c)
  | GMT -> string "GMT"
  | PST -> string "PST"
  | UT  -> string "UT"
  | EDT -> string "EDT"
  | PDT -> string "PDT"
  | CST -> string "CST"
  | CDT -> string "CDT"
  | MDT -> string "MDT"
  | MST -> string "MST"
  | EST -> string "EST"

  let w_date { day; date; time; zone } =
    hovbox 0
    $ (match day with
       | Some day -> w_day day $ string "," $ space
       | None -> noop)
    $ w_date date
    $ space
    $ w_time time
    $ space
    $ hovbox 0
    $ w_zone zone
    $ close_box
    $ close_box
end

module Decoder =
struct
  let p_hour        = Rfc5322.hour
  let p_minute      = Rfc5322.minute
  let p_second      = Rfc5322.second
  let p_year        = Rfc5322.year
  let p_month       = Rfc5322.month
  let p_day_of_week = Rfc5322.day_of_week
  let p_day         = Rfc5322.day
  let p_zone        = Rfc5322.zone
  let p_time        = Rfc5322.time
  let p_date        = Rfc5322.date
  let p_date_time   = Rfc5322.date_time
end

let to_string t =
  let buf   = Buffer.create 16 in
  let state = Encoder.make () in

  let rec loop = function
    | `Partial (s, i, l, k) ->
      Buffer.add_subbytes buf s i l;
      loop @@ (k l)
    | `Ok -> Buffer.contents buf
  in

  loop @@ (Wrap.lift Wrap.(Encoder.w_date t (unlift (Encoder.flush (fun _ -> `Ok))))) state

let of_string ?(chunk = 1024) s =
  let s' = s ^ "\r\n" in
  let l = String.length s' in
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (l - consumed) in
      Input.write_string buffer s' consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  aux 0 @@ Parser.run i Parser.(Rfc5322.date_time <* Rfc822.crlf)

let of_string_raw ?(chunk = 1024) s off len =
  let i = Input.create_bytes chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = min chunk (len - (consumed - off)) in
      Input.write_string buffer s consumed n;
      aux (consumed + n) @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some (v, consumed - off)
  in

  aux off @@ Parser.run i Rfc5322.date_time

let equal = (=)
