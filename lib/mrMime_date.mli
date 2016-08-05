(** Module Date *)

(** Day of week *)
type day   = Rfc5322.day =
  | Mon | Tue | Wed
  | Thu | Fri | Sat
  | Sun

(** Month *)
type month = Rfc5322.month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec

(** Timezone *)
type zone  = Rfc5322.zone =
  | UT                    (** Universal time (identical to [+0000]) *)
  | GMT                   (** Greenwich Mean time (identical to [+0000]) *)
  | EST                   (** Eastern Standard time (identical to [-0500]) *)
  | EDT                   (** Eastern Daylight Savings time (identical to
                              [-0400]) *)
  | CST                   (** Central Standard time (identical to [-0600]) *)
  | CDT                   (** Central Daylight Savings time (identical to
                              [-0500]) *)
  | MST                   (** Mountain Standard time (identical to [-0700]) *)
  | MDT                   (** Mountain Daylight Savings time (identical to
                              [-0600]) *)
  | PST                   (** Pacific Standard time (identical to [-0800]) *)
  | PDT                   (** Pacific Daylight Savings time (identical to
                              [-0700]) *)
  | Military_zone of char (** The character military zones were defined in a
                              non-standard way in
                              {{:https://tools.ietf.org/html/rfc822}RFC822} and
                              are therefore unpredictable in their meaning.

                              The original definitions of the military zones [A]
                              through [I] are equivalent to [+0100] through
                              [+0900], respectively; [K], [L], and [M] are
                              equivalent to [+1000], [+1100], and [+1200],
                              respectively; [N] through [Y] are equivalent to
                              [-0100] through [-1200] respectively; and [Z] is
                              equivalent to [+0000]. *)
  | TZ of int             (** [TZ +hhmm] means [+(hh * 60 + mm)] minutes, and
                              [TZ -hhmm] means [-(hh * 60 + mm)] minutes.

                              Accoding to the standard
                              {{:https://tools.ietf.org/html/rfc5322}RFC5322},
                              [mm] must be within the range 00 through 59.
                              MrMime does not check that. *)

(** Date *)
type date = Rfc5322.date =
  { day  : day option             (** Accoding to the standard
                                      {{:https://tools.ietf.org/html/rfc5322}RFC5322},
                                      the [day] (if included) must be the day
                                      implied by the date. MrMime does not check
                                      that. *)
  ; date : int * month * int      (** [(day, month, year)]:
                                      - [day] is the numeric day of the month.
                                        According to the standard
                                        {{:https://tools.ietf.org/html/rfc5322}RFC5322},
                                        the [day] must be between 1 and the
                                        number of days allowed for the specified
                                        month (in the specified year). MrMime
                                        does not check that.
                                      - [year] is any numeric year 1900 or
                                        later. *)
  ; time : int * int * int option (** According to the standard
                                      {{:https://tools.ietf.org/html/rfc5322}RFC5322},
                                      [time] must be in the range [00:00:00]
                                      through [23:59:60] (the number of seconds
                                      allowing for a leap second; see
                                      {{:https://tools.ietf.org/html/rfc1305}RFC1305}).
                                      MrMime does not check that. *)
  ; zone : zone }

(** [pp_zone] prints an human readable representation of [zone]. *)
val pp_zone       : Format.formatter -> zone -> unit

(** [pp_month] prints an human readable representation of [month]. *)
val pp_month      : Format.formatter -> month -> unit

(** [pp_day] prints an human readable representation of [day]. *)
val pp_day        : Format.formatter -> day -> unit

(** [pp date] prints an human readable representatation of [date]. *)
val pp            : Format.formatter -> date -> unit

module Encoder :
sig
  val w_day       : (day,                      'r Encoder.partial) Wrap.k1
  val w_date      : (date,                     'r Encoder.partial) Wrap.k1
  val w_time      : ((int * int * int option), 'r Encoder.partial) Wrap.k1
  val w_zone      : (zone,                     'r Encoder.partial) Wrap.k1
  val w_date      : (date,                     'r Encoder.partial) Wrap.k1
end

module Decoder :
sig
  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      hour              = 2DIGIT / obs-hour
      obs-hour          = [CFWS] 2DIGIT [CFWS]
      ]}
  *)
  val p_hour        : int MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      minute            = 2DIGIT / obs-minute
      obs-minute        = [CFWS] 2DIGIT [CFWS]
      ]}
  *)
  val p_minute      : int MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      second            = 2DIGIT / obs-second
      obs-second        = [CFWS] 2DIGIT [CFWS]
      ]}
  *)
  val p_second      : int MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      year              = (FWS 4*DIGIT FWS) / obs-year
      obs-year          = [CFWS] 2*DIGIT [CFWS]
      ]}
  *)
  val p_year        : int MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      day               = ([FWS] 1*2DIGIT FWS) / obs-day
      obs-day           = [CFWS] 1*2DIGIT [CFWS]
      ]}
  *)
  val p_day         : int MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3}:

      {[
      month             = "Jan" / "Feb" / "Mar" / "Apr" /
                          "May" / "Jun" / "Jul" / "Aug" /
                          "Sep" / "Oct" / "Nov" / "Dec"
      ]}
  *)
  val p_month       : month MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      day-of-week       = ([FWS] day-name) / obs-day-of-week
      obs-day-of-week   = [CFWS] day-name [CFWS]
      day-name          = "Mon" / "Tue" / "Wed" / "Thu" /
                          "Fri" / "Sat" / "Sun"
      ]}
  *)
  val p_day_of_week : day MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3} &
      {{:https://tools.ietf.org/html/rfc5322#section-4.3}4.3}:

      {[
      zone              = (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
      obs-zone          = "UT" / "GMT" /     ; Universal Time
                                             ; North American UT
                                             ; offsets
                          "EST" / "EDT" /    ; Eastern:  - 5/ - 4
                          "CST" / "CDT" /    ; Central:  - 6/ - 5
                          "MST" / "MDT" /    ; Mountain: - 7/ - 6
                          "PST" / "PDT" /    ; Pacific:  - 8/ - 7
                                             ;
                          %d65-73 /          ; Military zones - "A"
                          %d75-90 /          ; through "I" and "K"
                          %d97-105 /         ; through "Z", both
                          %d107-122          ; upper and lower case
      ]}
  *)
  val p_zone        : zone MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3}:

     {[
     time               = time-of-day zone
     time-of-day        = hour ":" minute [ ":" second ]
     ]}
  *)
  val p_time        : ((int * int * int option) * zone) MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3}:

      {[
      date              = day month year
      ]}
  *)
  val p_date        : (int * month * int) MrMime_parser.t

  (** See RFC 5322 § {{:https://tools.ietf.org/html/rfc5322#section-3.3}3.3}:

      {[
      date-time         = [ day-of-week "," ] date time [CFWS]
      ]}
  *)
  val p_date_time   : date MrMime_parser.t
end

(** [to_string date] formats the date [date] accoding to a
    {{:https://tools.ietf.org/html/rfc5322#section-3.3}RFC5322}. *)
val to_string     : date -> string


(** [of_string ~chunk:1024 buf] parses an
    {{:https://tools.ietf.org/html/rfc5322#section-3.3}RFC5322} date starting at
    0 in [buf].

    This function allocates a internal buffer with [chunk] size (default to
    [1024]).
*)
val of_string     : ?chunk:int -> string -> date option

(** [of_string_raw ~chunk:1024 buf off len] parses an
    {{:https://tools.ietf.org/html/rfc5322#section-3.3}RFC5322} date starting at
    [off] in [buf] to a tuple [(date, count)] with:
    - [date] the date
    - [count] the number of bytes read starting at [off] to parse the date.

    This function allocates a internal buffer with [chunk] size (default to
    [1024]).
*)
val of_string_raw : ?chunk:int -> string -> int -> int -> (date * int) option

(** [equal a b] is [true] iff [a] and [b] are the same date. *)
val equal         : date -> date -> bool
