type day   = Rfc5322.day
type month = Rfc5322.month
type tz    = Rfc5322.tz

let pp = Format.fprintf

let pp_tz fmt = function
  | `Military_zone c -> pp fmt "%c" c
  | `TZ i -> pp fmt "TZ %04d" i
  | `GMT  -> pp fmt "GMT"
  | `PST  -> pp fmt "PST"
  | `UT   -> pp fmt "UT"
  | `EDT  -> pp fmt "EDT"
  | `PDT  -> pp fmt "PDT"
  | `CST  -> pp fmt "CST"
  | `CDT  -> pp fmt "CDT"
  | `MDT  -> pp fmt "MDT"
  | `MST  -> pp fmt "MST"
  | `EST  -> pp fmt "EST"

let pp_month fmt = function
  | `Feb -> pp fmt "Feb"
  | `Mar -> pp fmt "Mar"
  | `Dec -> pp fmt "Dec"
  | `Jul -> pp fmt "Jul"
  | `Sep -> pp fmt "Sep"
  | `Nov -> pp fmt "Nov"
  | `Aug -> pp fmt "Aug"
  | `Jun -> pp fmt "Jun"
  | `May -> pp fmt "May"
  | `Apr -> pp fmt "Apr"
  | `Oct -> pp fmt "Oct"
  | `Jan -> pp fmt "Jan"

let pp_day fmt = function
  | `Sat -> pp fmt "Sat"
  | `Fri -> pp fmt "Fri"
  | `Mon -> pp fmt "Mon"
  | `Wed -> pp fmt "Wed"
  | `Sun -> pp fmt "Sun"
  | `Tue -> pp fmt "Tue"
  | `Thu -> pp fmt "Thu"

type t =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; tz   : tz }

let pp fmt = function
  | { day = Some day; date = (d, m, y); time = (hh, mm, ss); tz; } ->
    pp fmt "{ @[<hov>day = %a;@ date = (@[<hov>%d,@ %a,@ %d@])@ time = (@[<hov>%d,@ %d,@ %d@])@ tz = %a@] }"
      pp_day day d pp_month m y hh mm (Option.value ~default:0 ss) pp_tz tz
  | { day = None; date = (d, m, y); time = (hh, mm, ss); tz; } ->
    pp fmt "{ @[<hov>date = (@[<hov>%d,@ %a,@ %d@])@ time = (@[<hov>%d,@ %d,@ %d@])@ tz = %a@] }"
      d pp_month m y hh mm (Option.value ~default:0 ss) pp_tz tz

module D =
struct
  let of_lexer (day, (d, m, y), (hh, mm, ss), tz) =
    { day; date = (d, m, y); time = (hh, mm, ss); tz; }

  open BaseDecoder

  let of_decoder state =
    let rec loop = function
      | `Error (exn, buf, off, len) ->
        let tmp = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer tmp in

        Format.fprintf fmt "%a (buf: %S)%!"
          Error.pp exn (Bytes.sub buf off (len - off));

        raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
      | `Read (buf, off, len, k) ->
        raise (Invalid_argument "Address.of_string: unterminated string")
      | `Ok data -> of_lexer data
    in

    let rule =
      Rfc5322.p_date_time
      @ fun date -> Rfc822.p_crlf
      @ Rfc822.p_crlf
      @ fun _ -> `Ok date
    in

    loop @@ safe rule state
end

module E =
struct
  module Internal =
  struct
    open BaseEncoder
    open Wrap

    let w_day = function
      | `Sat -> w_string "Sat"
      | `Fri -> w_string "Fri"
      | `Mon -> w_string "Mon"
      | `Wed -> w_string "Wed"
      | `Sun -> w_string "Sun"
      | `Tue -> w_string "Tue"
      | `Thu -> w_string "Thu"

    let w_date (day, month, year) =
      w_hovbox 1
      $ w_string (string_of_int day)
      $ w_close_box
      $ w_space
      $ w_hovbox 1
      $ (match month with
         | `Feb -> w_string "Feb"
         | `Mar -> w_string "Mar"
         | `Dec -> w_string "Dec"
         | `Jul -> w_string "Jul"
         | `Sep -> w_string "Sep"
         | `Nov -> w_string "Nov"
         | `Aug -> w_string "Aug"
         | `Jun -> w_string "Jun"
         | `May -> w_string "May"
         | `Apr -> w_string "Apr"
         | `Oct -> w_string "Oct"
         | `Jan -> w_string "Jan")
      $ w_close_box
      $ w_space
      $ w_hovbox 1
      $ w_string (string_of_int year)
      $ w_close_box

    let w_time (hh, mm, ss) =
      w_hovbox 1
      $ w_string (sp "%02d" hh)
      $ w_close_box
      $ w_string ":"
      $ w_hovbox 1
      $ w_string (sp "%02d" mm)
      $ w_close_box
      $ match ss with
        | Some ss -> w_string ":" $ w_hovbox 1 $ w_string (sp "%02d" ss) $ w_close_box
        | None -> noop

    let w_tz = function
    | `TZ zone -> w_string (sp "%c%04d" (if zone < 0 then '-' else '+') (abs zone))
    | `Military_zone c -> w_string (sp "%c" c)
    | `GMT -> w_string "GMT"
    | `PST -> w_string "PST"
    | `UT  -> w_string "UT"
    | `EDT -> w_string "EDT"
    | `PDT -> w_string "PDT"
    | `CST -> w_string "CST"
    | `CDT -> w_string "CDT"
    | `MDT -> w_string "MDT"
    | `MST -> w_string "MST"
    | `EST -> w_string "EST"

    let w_date { day; date; time; tz } =
      w_hovbox 1
      $ (match day with
         | Some day -> w_day day $ w_string "," $ w_space
         | None -> noop)
      $ w_date date
      $ w_space
      $ w_time time
      $ w_space
      $ w_hovbox 1
      $ w_tz tz
      $ w_close_box
      $ w_close_box
  end

  let w = Internal.w_date

  let to_buffer date state =
    let buf = Buffer.create 16 in

    let rec loop = function
      | `Partial (s, i, l, k) ->
        Buffer.add_subbytes buf s i l;
        loop @@ (k l)
      | `Ok -> buf
    in

    let rule =
      let open BaseEncoder in
      let ok = flush (fun _ -> `Ok) in
      Wrap.lift Wrap.(Internal.w_date date (unlift ok))
    in

    loop @@ rule state
end

let of_string s = D.of_decoder (Decoder.of_string (s ^ "\r\n\r\n"))
let to_string d = Buffer.contents @@ E.to_buffer d (Encoder.make ())

let equal = (=)
