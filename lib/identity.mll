rule identity buf = parse
  | _ as chr { Buffer.add_char buf chr; identity buf lexbuf }
  | eof      { () }

{
  module Encode = Flow.Make(struct let conv = identity end)
  module Decode = Flow.Make(struct let conv = identity end)
}
