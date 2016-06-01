open BasePrinter

type field = Grammar.field

type t = Header.unstrict *
  [ `Composite of Content.t * nest option list
  | `Discrete of Content.t * string ]
and nest =
  [ `Composite of (Content.t * field list) * 'a option list
  | `Discrete of (Content.t * field list) * string ] as 'a

let of_string s : t =
  let rec loop = function
    | `Error (err, buf, off, len) ->
      raise (Error.Error (`Error (err, buf, off, len)))
    | `Read (buf, off, len, k) ->
      raise (Error.Error (Error.err_nothing_to_do { Decoder.buffer = buf; pos = off; len; }))
    | `Ok data -> data
  in

  let rule = Grammar.p_message (fun header message state -> `Ok (header, message)) in
  loop @@ BaseDecoder.safe rule (Decoder.of_string s)

let equal = (=)

let pp fmt (_ : t) = p fmt "#message"
