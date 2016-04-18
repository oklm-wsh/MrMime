type day   = Rfc5322.day
type month = Rfc5322.month
type tz    = Rfc5322.tz

type t =
  { day  : day option
  ; date : int * month * int
  ; time : int * int * int option
  ; tz   : tz }

let of_lexer (day, (d, m, y), (hh, mm, ss), tz) =
  { day; date = (d, m, y); time = (hh, mm, ss); tz; }

let of_string s =
  let rec loop = function
    | `Error (exn, buf, off, len) ->
      let tmp = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer tmp in

      Format.fprintf fmt "%a (buf: %S)%!"
        Lexer.pp_error exn (Bytes.sub buf off (len - off));

      raise (Invalid_argument ("Address.of_string: " ^ (Buffer.contents tmp)))
    | `Read (buf, off, len, k) ->
      raise (Invalid_argument "Address.of_string: unterminated string")
    | `Ok data -> of_lexer data
  in

  let rule = Rfc5322.p_date_time (fun data state -> `Ok data) in
  loop @@ Lexer.safe rule (Lexer.of_string (s ^ "\r\n\r\n"))

let p = Format.fprintf

let pp_day fmt = function
  | Some `Mon -> p fmt "Mon, "
  | Some `Tue -> p fmt "Tue, "
  | Some `Wed -> p fmt "Wed, "
  | Some `Thu -> p fmt "Thu, "
  | Some `Fri -> p fmt "Fri, "
  | Some `Sat -> p fmt "Sat, "
  | Some `Sun -> p fmt "Sun, "
  | None -> ()

let pp_month fmt = function
  | `Jan -> p fmt "Jan"
  | `Feb -> p fmt "Feb"
  | `Mar -> p fmt "Mar"
  | `Apr -> p fmt "Apr"
  | `May -> p fmt "May"
  | `Jun -> p fmt "Jun"
  | `Jul -> p fmt "Jul"
  | `Aug -> p fmt "Aug"
  | `Sep -> p fmt "Sep"
  | `Oct -> p fmt "Oct"
  | `Nov -> p fmt "Nov"
  | `Dec -> p fmt "Dec"

let pp_second fmt = function
  | Some ss -> p fmt ":%02d" ss
  | None -> ()

let pp_tz fmt = function
  | `TZ n -> p fmt "%c%04d" (if n < 0 then '-' else '+') (abs n)
  | `UT -> p fmt "UT"
  | `GMT -> p fmt "GMT"
  | `EST -> p fmt "EST"
  | `EDT -> p fmt "EDT"
  | `CST -> p fmt "CST"
  | `CDT -> p fmt "CDT"
  | `MST -> p fmt "MST"
  | `MDT -> p fmt "MDT"
  | `PST -> p fmt "PST"
  | `PDT -> p fmt "PDT"
  | `Military_zone chr -> p fmt "%c" chr

let pp fmt { day; date = (d, m, y); time = (hh, mm, ss); tz } =
  p fmt "%a%02d %a %04d %02d:%02d%a %a"
    pp_day day d pp_month m y hh mm pp_second ss pp_tz tz

let to_string date =
  let tmp = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer tmp in

  Format.fprintf fmt "%a%!" pp date;
  Buffer.contents tmp

let equal = (=)
