open BaseDecoder

let p_vchar p state =
  let is_vchar = function
    | '\x21' .. '\x7e' -> true
    | _ -> false
  in

  let rec loop decoder = match Uutf.decode decoder with
    | `End -> assert false (* TODO! *)
    | `Uchar uchar -> p uchar
    | `Malformed _ ->
      [%debug Printf.printf "stte: p_vchar (RFC 6532) malformer\n%!"];
      fun state -> raise (Error.Error (Error.err_malformed_sequence state))
    | `Await ->
      cur_chr @ function
      | '\x00' .. '\x7F' as chr ->
        if is_vchar chr
        then junk_chr @ p (Char.code chr)
        else fun state -> raise (Error.Error (Error.err_unexpected chr state))
      | uchr -> Uutf.Manual.src decoder (String.make 1 uchr) 0 1;
                junk_chr @ loop decoder
  in

  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in

  loop decoder state

let p_ctext = p_vchar
let p_atext = p_vchar
let p_qtext = p_vchar
let p_dtext = p_vchar
let p_text  = p_vchar

let to_string uchar =
  let buf = Buffer.create 8 in

  Uutf.Buffer.add_utf_8 buf uchar;
  Buffer.contents buf
