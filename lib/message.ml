open BasePrinter

type field = Grammar.field

type top = Header.t *
  [ `Composite of Content.t * nest option list
  | `Discrete of Content.t * string ]
and nest =
  [ `Composite of (Content.t * field list) * 'a option list
  | `Discrete of (Content.t * field list) * string ] as 'a

let of_string s : top =
  let rec loop = function
    | `Error (err, buf, off, len) ->
      raise (Error.Error (`Error (err, buf, off, len)))
    | `Read (buf, off, len, k) ->
      raise (Error.Error (Error.err_nothing_to_do { Lexer.buffer = buf; pos = off; len; }))
    | `Ok data -> data
  in

  let rule = Grammar.p_message (fun header message state -> `Ok (header, message)) in
  loop @@ BaseLexer.safe rule (Lexer.of_string s)
