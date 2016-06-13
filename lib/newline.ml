let newline =
  if Sys.win32
  then "\r\n"
  else "\n"

let is_lf =
  if Sys.win32
  then false
  else true

let is_crlf =
  if Sys.win32
  then true
  else false
