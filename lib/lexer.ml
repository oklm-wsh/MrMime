(* let () = Logs.set_level ~all:true (Some Logs.Debug) *)
let () = Logs.set_reporter (Logs_fmt.reporter ())

type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int
  ; mutable len    : int }

let make ?(len = 1024) () =
  { buffer = Bytes.create len
  ; pos    = 0
  ; len    = 0 }

let of_string str =
  { buffer = Bytes.of_string str
  ; pos    = 0
  ; len    = String.length str }

let of_bytes bytes =
  { buffer = bytes
  ; pos    = 0
  ; len    = Bytes.length bytes }
