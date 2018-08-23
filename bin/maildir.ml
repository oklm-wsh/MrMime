module Practicality = String
open Astring
open MrMime

let () = Fmt.set_style_renderer Fmt.stdout `Ansi_tty

type newline = CRLF | LF

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
  | CRLF -> input channel buf off len
  | LF ->
    let rec read_char has_cr remaining =
      assert (remaining >= 0);

      if remaining = 0 then len
      else match input_char channel with
           | '\n' when not has_cr && remaining >= 2 ->
             Bytes.blit (Bytes.of_string "\r\n") 0 buf (last - remaining) 2;
             read_char false (remaining - 2)
           | '\n' when not has_cr && remaining = 1 ->
             let pos = pos_in channel in
             seek_in channel (pred pos);
             (len - remaining)
           | '\r' ->
             Bytes.set buf (last - remaining) '\r';
             read_char true  (pred remaining)
           | chr  ->
             Bytes.set buf (last - remaining) chr;
             read_char false (pred remaining)
           | exception End_of_file -> (len - remaining)
    in

    read_char false len

let of_filename filename =
  open_in filename

let to_filename filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch

let message ?(chunk = 1024) ?(newline = LF) input =
  let i = Input.create_bytes chunk in
  let t = Bytes.create chunk in

  let rec aux consumed = function
    | Parser.Fail _ -> None
    | Parser.Read { buffer; k; } ->
      let n = read_into ~newline input t 0 chunk in
      Input.write buffer (Internal_buffer.from_bytes t) 0 n;
      aux (consumed + n)
      @@ k n (if n = 0 then Parser.Complete else Parser.Incomplete)
    | Parser.Done v -> Some v
  in

  let v = aux 0 @@ Parser.run i Top.message in
  close_in input; v

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
  let one filename =
     Fmt.pf Fmt.stdout "%a%a%!"
       (left left_c yellow_s) "..."
       (left 20 cyan_s) filename;

     match message ~newline @@ of_filename filename with
     | Some v ->
         print (fun ppf -> Fmt.string ppf "\r");
         Fmt.pf Fmt.stdout "%a%a\n%!"
           (left left_c green_s) "[OK]"
           (left 20 cyan_s) filename;
     | None ->
          print (fun ppf -> Fmt.string ppf "\r");
          Fmt.pf Fmt.stdout "%a%a\n%!"
            (left left_c red_s) "[ERROR]"
            cyan_s filename
  in

  List.iter one files

let path =
  let doc = "Path of mail directory." in
  Arg.(required & opt (some string) None & info ["p"; "path"] ~doc)

let newline =
  let parse s =
    match Practicality.uppercase_ascii s with
    | "CRLF" -> `Ok CRLF
    | "LF" -> `Ok LF
    | _ -> `Error "Invalid newline."
  in
  let pp fmt = function
    | CRLF -> Format.fprintf fmt "CRLF"
    | LF -> Format.fprintf fmt "LF"
  in
  parse, pp

let newline =
  let doc = "Specify a specific newline." in
  Arg.(value & opt newline LF & info ["n"; "newline"] ~doc)

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
