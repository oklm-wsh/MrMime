open Lwt.Infix
open Astring

let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty

type newline =
  | CRLF
  | CR
  | LF

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let terminal_columns =
  try
    (* terminfo *)
    with_process_in "tput cols" (fun ic -> int_of_string (input_line ic))
  with _ -> try
      (* GNU stty *)
      with_process_in "stty size" (fun ic ->
          match String.cuts (input_line ic) ~sep:" " with
          | [_ ; v] -> int_of_string v
          | _ -> failwith "stty")
    with _ -> try
        (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with _ ->
        (* default *)
        80

let left_c = 20

let line ppf ?color c =
  let line = String.v ~len:terminal_columns (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string)  line
  | None   -> Fmt.pf ppf "%s\n%!"line

let left nb pp ppf a =
  let s = Fmt.to_to_string pp a in
  let nb = nb - String.length s in
  if nb <= 0 then pp ppf a
  else begin
    pp ppf a;
    Fmt.string ppf (String.v ~len:nb (fun _ -> ' '))
  end

let print k = k Fmt.stdout

let color c ppf fmt = Fmt.(styled c string) ppf fmt
let red_s fmt = color `Red fmt
let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt
let green_s fmt = color `Green fmt
let yellow_s fmt = color `Yellow fmt
let bold_s fmt = color `Bold fmt
let cyan_s fmt = color `Cyan fmt

let read_into ?(newline = LF) channel buf off len =
  if len + off > Bytes.length buf
     || off < 0
     || len < 0
  then raise (Invalid_argument "index out of bound");

  let last = len + off in

  match newline with
  | CRLF -> Lwt_io.read_into channel buf off len
  | CR ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then Lwt.return len
      else Lwt.catch
             (fun () -> Lwt_io.read_char channel >>= fun chr -> Lwt.return (Some chr))
             (fun exn -> Lwt.return None)
           >>= function
           | Some '\n' when has_cr ->
             read_char false remaining
           | Some '\r' ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char true  (remaining - 2)
           | Some chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | None -> Lwt.return (len - remaining)
    in

    read_char false len
  | LF ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then Lwt.return len
      else Lwt.catch
             (fun () -> Lwt_io.read_char channel >>= fun chr -> Lwt.return (Some chr))
             (fun exn -> Lwt.return None)
           >>= function
           | Some '\n' when not has_cr && remaining >= 2 ->
             Bytes.blit "\r\n" 0 buf (last - remaining) 2;
             read_char false (remaining - 2)
           | Some '\n' when not has_cr && remaining = 1 ->
             let pos = Lwt_io.position channel in
             Lwt_io.set_position channel (Int64.pred pos) >>= fun () ->
             Lwt.return (len - remaining)
           | Some '\r' ->
             Bytes.set buf (last - remaining) '\r';
             read_char true  (pred remaining)
           | Some chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | None -> Lwt.return (len - remaining)
    in

    read_char false len

let rec of_flow (ch, decoder) newline =
  let rec aux = function
    | `Read (buff, off, len, k) ->
      read_into ~newline ch buff off len >>= fun n ->
      aux (k n)
    | `Error (err, buff, off, len) ->
      Lwt.fail (Error.Error (`Error (err, buff, off, len)))
    | `Ok message -> Lwt.return message
  in

  aux

let of_filename filename = Lwt_io.open_file ~mode:Lwt_io.Input filename

let message filename newline input =
  Fmt.pf Fmt.stdout "%a%a%!" (left left_c yellow_s) "..." cyan_s filename;

  let state = Decoder.make () in
  Lwt.catch
    (fun () ->
     of_flow (input, state) newline
       (Grammar.p_message
       (fun header message _ -> `Ok (header, message)) state)
     >>= fun _ ->
         print (fun ppf -> Fmt.string ppf "\r");
         Fmt.pf Fmt.stdout "%a%a\n%!"
           (left left_c green_s) "[OK]"
           (left 20 cyan_s) filename;
         Lwt.return ())
    (function (Error.Error `Error (_, buf, off, len)) as exn ->
              print (fun ppf -> Fmt.string ppf "\r");
              Fmt.pf Fmt.stdout "%a%a %a\n%!"
                (left left_c red_s) "[ERROR]"
                cyan_s filename
                yellow_s (Printexc.to_string exn);
              Lwt.return ()
            | exn ->
              print (fun ppf -> Fmt.string ppf "\r");
              Fmt.pf Fmt.stdout "%a%a %a\n%!"
                (left left_c red_s) "[ERROR]"
                cyan_s filename
                yellow_s (Printexc.to_string exn);
              Lwt.return ())
  >>= fun () -> Lwt_io.close input

let convert input =
  let buffer = Bytes.create 1024 in

  let rec aux off len =
    read_into input buffer off len >>= function
    | 0 -> Lwt.return ()
    | n -> Printf.printf "%s%!" (Bytes.sub buffer off n);
           if off + n = 1024
           then aux 0 1024
           else aux (off + n) (len - n)
  in

  aux 0 1024

open Cmdliner

let walk directory pattern =
  let select str = Re_str.string_match (Re_str.regexp pattern) str 0 in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs, files) kind ->
          match (Unix.stat kind).Unix.st_kind with
          | Unix.S_REG -> (dirs, kind :: files)
          | Unix.S_DIR -> (kind :: dirs, files)
          | _ -> (dirs, files))
          ([], []) contents
      in
      let matched = List.filter select files in
      aux (matched @ acc) (dirs @ rest)
  in
  aux [] [directory]

let do_cmd path newline =
  let files = walk path ".+" in
  Lwt_main.run
    (Lwt_list.map_s (fun x -> of_filename x >>= message x newline) files)

let path =
  let doc = "Path of mail directory." in
  Arg.(required & opt (some string) None & info ["p"; "path"] ~doc)

let newline =
  let parse s =
    match Bytes.uppercase_ascii s with
    | "CRLF" -> `Ok CRLF
    | "CR" -> `Ok CR
    | "LF" -> `Ok LF
    | _ -> `Error "Invalid newline."
  in
  let pp fmt = function
    | CRLF -> Format.fprintf fmt "CRLF"
    | CR -> Format.fprintf fmt "CR"
    | LF -> Format.fprintf fmt "LF"
  in
  parse, pp

let newline =
  let doc = "Specify a specific newline." in
  Arg.(value & opt newline CRLF & info ["n"; "newline"] ~doc)

let cmd =
  let doc = "Scan mail directory and try to parse emails." in
  let man =
    [ `S "Description"
    ; `P "$(tname) is test tool." ] in
  Term.(pure do_cmd $ path $ newline),
  Term.info "maildir" ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
