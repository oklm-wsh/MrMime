{
  let hex a b =
    let one c = match c with
      | '0' .. '9' -> (Char.code c) - (Char.code '0') + 0
      | 'A' .. 'F' -> (Char.code c) - (Char.code 'A') + 10
      | 'a' .. 'f' -> (Char.code c) - (Char.code 'a') + 10
      | _ -> assert false
    in
    Char.chr (((one a) * 16) + (one b))
}

let qp_allowed = [ '\033' - '\060' '\062' - '\126' ]
let crlf       = '\r' '\n'
let wsp        = [ ' ' '\t' ]
let hex        = [ '0' - '9' 'a' - 'f' 'A' - 'F' ]

rule decode buffer k_done k_eof = parse
 | wsp * crlf
   { k_done `Hard buffer lexbuf }
 | '=' crlf
   { k_done `Soft buffer lexbuf }
 | '=' (hex as a) (hex as b)
   { Buffer.add_char buffer (hex a b);
     decode buffer k_done k_eof lexbuf }
 | (qp_allowed | wsp)+ as str
   { Buffer.add_string buffer str;
     decode buffer k_done k_eof lexbuf }
 | _
   { decode buffer k_done k_eof lexbuf }
 | eof { k_eof buffer lexbuf }
