let locate buff off len f =
  let idx = ref 0 in
  while !idx < len && f (Internal_buffer.get buff (off + !idx))
  do incr idx done;

  !idx

open Parser

type err += Empty_string

let str f =
  { f = fun i s fail succ ->
    let utf8 = Buffer.create 16 in
    let decoder = Uutf.decoder `Manual in

    let decode buff off len =
      let len' = locate buff off len (function '\000' .. '\127' as chr -> f chr | _ -> true) in
      Uutf.Manual.src decoder (Internal_buffer.sub_string buff off len') 0 len';
      len'
    in

    let rec loop consumed i s =
      match Uutf.decode decoder with
      | `End ->
        if Buffer.length utf8 > 0
        then succ i s (Buffer.contents utf8)
        else fail i s [] Empty_string
      | `Uchar uchar ->
        Uutf.Buffer.add_utf_8 utf8 uchar;
        loop consumed i s
      | `Malformed _ ->
        Uutf.Buffer.add_utf_8 utf8 Uutf.u_rep;
        if consumed - (Uutf.decoder_byte_count decoder) = 0
        then succ i s (Buffer.contents utf8)
        else loop consumed i s
      | `Await when Input.ravailable i = 0 && s = Incomplete ->
        IO.prompt i (fun i s ->
                  if Buffer.length utf8 > 0
                  then loop consumed i s
                  else fail i s [] Empty_string)
                 (loop consumed)
      | `Await when Input.ravailable i = 0 && s = Complete ->
        Uutf.Manual.src decoder Bytes.empty 0 0;
        loop consumed i s

        (*
        if consumed - (Uutf.decoder_byte_count decoder) = 0
        then (if Buffer.length utf8 > 0
              then succ i s (Buffer.contents utf8)
              else fail i s [] Empty_string)
        else loop consumed i s
        *)
      | `Await ->
        let consumed' = Input.transmit i decode in
        let consumed  = consumed' + consumed in

        if consumed - (Uutf.decoder_byte_count decoder) = 0
        then (if Buffer.length utf8 > 0
              then succ i s (Buffer.contents utf8)
              else fail i s [] Empty_string)
        else loop consumed i s
    in

    loop 0 i s }

(* Remove the constraint « at least, one character » from [str] *)
let dtext is_dtext =
  { f = fun i s _fail succ ->
    let utf8 = Buffer.create 16 in
    let decoder = Uutf.decoder `Manual in

    let decode buff off len =
      let len' =
        locate buff off len
          (function '\000' .. '\127' as chr -> is_dtext chr | _ -> true) in
      Uutf.Manual.src decoder (Internal_buffer.sub_string buff off len') 0 len';
      len'
    in

    let rec loop consumed i s =
      match Uutf.decode decoder with
      | `End ->
        succ i s (Buffer.contents utf8)
      | `Uchar uchar ->
        Uutf.Buffer.add_utf_8 utf8 uchar;
        loop consumed i s
      | `Malformed _ ->
        Uutf.Buffer.add_utf_8 utf8 Uutf.u_rep;
        if consumed - (Uutf.decoder_byte_count decoder) = 0
        then succ i s (Buffer.contents utf8)
        else loop consumed i s
      | `Await when Input.ravailable i = 0 ->
        IO.prompt i (fun i s -> loop consumed i s) (loop consumed)
      | `Await ->
        let consumed = consumed + Input.transmit i decode in

        if consumed - (Uutf.decoder_byte_count decoder) = 0
        then succ i s (Buffer.contents utf8)
        else loop consumed i s
    in

    loop 0 i s }
