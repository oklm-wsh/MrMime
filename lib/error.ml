type error =
  [ `Unexpected_eoi
  | `Expected_char       of char
  | `Expected_set        of char list
  | `Unexpected_char     of char
  | `Unexpected_str      of string
  | `Expected_str        of string
  | `Wrong_padding
  | `Unexpected_encoding of string
  | `Invalid_ipv6
  | `Invalid_ipv4
  | `Invalid_ipv4v6
  | `Invalid_tag         of string
  | `Invalid_field       of string
  | `Nothing_to_do
  | `Invalid_header
  | `Unexpected_field    of string
  | `Invalid_boundary    of string
  | `Expected_boundary
  | `Malformed_sequence ]

let err e state                       = `Error
                                        (e, state.Decoder.buffer,
                                            state.Decoder.pos,
                                            state.Decoder.len)
let err_unexpected_eoi state          = err `Unexpected_eoi state
let err_expected chr state            = err (`Expected_char chr) state
let err_expected_set set state        = err (`Expected_set set) state
let err_unexpected chr state          = err (`Unexpected_char chr) state
let err_unexpected_str str state      = err (`Unexpected_str str) state
let err_expected_str str state        = err (`Expected_str str) state
let err_wrong_padding state           = err `Wrong_padding state
let err_unexpected_encoding str state = err (`Unexpected_encoding str) state
let err_invalid_ipv6 state            = err `Invalid_ipv6 state
let err_invalid_ipv4 state            = err `Invalid_ipv4 state
let err_invalid_ipv4v6 state          = err `Invalid_ipv4v6 state
let err_invalid_tag tag state         = err (`Invalid_tag tag) state
let err_invalid_field field state     = err (`Invalid_field field) state
let err_nothing_to_do state           = err `Nothing_to_do state
let err_invalid_header state          = err `Invalid_header state
let err_unexpected_field field state  = err (`Unexpected_field field) state
let err_invalid_boundary bound state  = err (`Invalid_boundary bound) state
let err_expected_boundary state       = err `Expected_boundary state
let err_malformed_sequence state      = err `Malformed_sequence state

let p = Format.fprintf

let pp_lst ?(sep = "") pp_data fmt set =
  let rec aux = function
    | [] -> ()
    | [ x ] -> p fmt "%a" pp_data x
    | x :: r -> p fmt "%a%s" pp_data x sep; aux r
  in
  aux set

let pp_char fmt = p fmt "%c"

let pp fmt = function
  | `Unexpected_eoi          -> p fmt "Unexpected EOI"
  | `Expected_char chr       -> p fmt "Expected [%S]" (String.make 1 chr)
  | `Expected_set set        -> p fmt "Expected [%a]"
                                  (pp_lst ~sep:" | " pp_char) set
  | `Unexpected_char chr     -> p fmt "Unexpected [%S]" (String.make 1 chr)
  | `Unexpected_str str      -> p fmt "Unexpected [%S]" str
  | `Expected_str str        -> p fmt "Expected [%S]" str
  | `Wrong_padding           -> p fmt "Wrong padding"
  | `Unexpected_encoding str -> p fmt "Unexpected encoding [%S]" str
  | `Invalid_ipv6            -> p fmt "Invalid IPv6"
  | `Invalid_ipv4            -> p fmt "Invalid IPv4"
  | `Invalid_ipv4v6          -> p fmt "Invalid IPv4 or IPv6"
  | `Invalid_tag tag         -> p fmt "Invalid tag [%s]" tag
  | `Invalid_field field     -> p fmt "Invalid field [%s]" field
  | `Nothing_to_do           -> p fmt "Nothing to do at this point"
  | `Invalid_header          -> p fmt "Invalid header"
  | `Unexpected_field field  -> p fmt "Unexpected field [%s]" field
  | `Invalid_boundary bound  -> p fmt "Invalid boundary [%S]" bound
  | `Expected_boundary       -> p fmt "Expected boundary parameter"
  | `Malformed_sequence      -> p fmt "Malformed sequence"

type     err = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

let pp_error fmt = function
  | `Error (err, buffer, pos, len) ->
    p fmt "Error: %a" pp err

exception Error of err

let () = Printexc.register_printer
  (function Error err ->
     let buf = Buffer.create 16 in
     let fmt = Format.formatter_of_buffer buf in
     Format.fprintf fmt "%a%!" pp_error err;
     Some (Buffer.contents buf)
   | _ -> None)
