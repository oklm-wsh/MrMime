let header = (module Message : Alcotest.TESTABLE with type t = Message.t)

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

let make_compute_test (id, s) =
  Printf.sprintf "message %d" id,
  `Slow,
  (fun () ->
   Printf.printf "%s%!" s;
   let _ = Message.of_string s in ())

let from_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  filename, Bytes.to_string s

let tests = List.map from_file (walk "messages" "[^ \.].*")

open Lwt.Infix
open Jackson

let server_cert = "./certificates/server.pem"
let server_key  = "./certificates/server.key"

type ret = [ `List of (int * int) list | `Body of string | `Ok | `Err ]

let rec process (ic, oc, state) status : ret Lwt.t =
  match status with
  | `Read (s, i, l, k) ->
    Lwt_io.read_into ic s i l >>= fun n ->
    process (ic, oc, state) (k n)
  | `Write (o, p, l, k) ->
    Lwt_io.write_from oc o p l >>= fun n ->
    process (ic, oc, state) (k n)
  | `Error _ as err -> Lwt.fail (Pop.Error.Error err)
  | #ret as value   -> Lwt.return value

let p = Printf.sprintf

let connect ?(port = 995) host =
  X509_lwt.authenticator (`Ca_dir "./certificates") >>= fun authenticator ->
  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun certificate ->
  Tls_lwt.connect_ext
    ~trace:(fun _ -> ())
    Tls.Config.(client ~authenticator ~certificates:(`Single certificate) ())
    (host, port) >>= fun (ic, oc) ->
  let state, value = Pop.connection () in
  process (ic, oc, state) value >>= function
    | `Ok -> Lwt.return (ic, oc, state)
    | _   -> Lwt.fail (Failure (p "Something is wrong with %s:%d" host port))

let p_command () = function
  | `List _ -> "LIST"
  | `Stat   -> "STAT"
  | `Quit   -> "QUIT"
  | `Retr _ -> "RETR"
  | `Dele _ -> "DELE"
  | `Noop   -> "NOOP"
  | `Rset   -> "RSET"
  | `User _ -> "USER"
  | `Pass _ -> "PASS"

let run command (ic, oc, state) =
  Printf.printf "RUN %s\n%!" (p_command () command);

  process (ic, oc, state) (Pop.run state command) >>= function
  | `Err    -> Lwt.fail (Failure (p "Error with command %a" p_command command))
  | `Ok     -> Lwt.return (`Ok, (ic, oc, state))
  | `Body s -> Lwt.return (`Text s, (ic, oc, state))
  | `List l -> Lwt.return (`List l, (ic, oc, state))

let download ?limit ?port host username password =
  let ( >?= ) ret next = ret >>= function
    | `Ok, state -> next state
    | _          -> Lwt.fail (Failure (p "Unexpected value"))
  in
  let ( >|= ) ret next = ret >>= function
    | `List l, state -> next l state
    | _          -> Lwt.fail (Failure (p "Unexpected value"))
  in
  let ( >:= ) ret next = ret >>= function
    | `Text s, state -> next s state
    | _              -> Lwt.fail (Failure (p "Unexpected value"))
  in
  connect ?port host
  >>= (run (`User username))
  >?= (run (`Pass password))
  >?= (run (`List None))
  >|= fun l state ->
      Lwt_list.fold_left_s
        (fun (state, limit, acc) (msg, _) ->
         match limit with
         | Some x when x > 0 ->
           run (`Retr msg) state >:= fun s state ->
           Lwt.return (state, Some (x - 1), (msg, s) :: acc)
         | None ->
           run (`Retr msg) state >:= fun s state ->
           Lwt.return (state, None, (msg, s) :: acc)
         | Some x -> Lwt.return (state, Some 0, acc))
        (state, limit, []) l
  >>= fun (state, _, ret) -> run `Quit state
  >>= fun _ -> Lwt.return ret

let resize n l=
  List.fold_left (fun (n, acc) x -> if n > 0 then (n - 1, x :: acc) else (0, acc)) (n, []) l
  |> fun (n, l) -> List.rev l

let value ~default = function Some x -> x | None -> default

let do_cmd user pass port host number =
  let thread () =
    download ?limit:number ~port host user pass
    >>= fun l ->
      Alcotest.run "Message test" ~argv:[|"message_test"|]
      [ "compute", List.map make_compute_test (resize (value ~default:(List.length l) number) l) ];
      Lwt.return ()
  in

  Lwt_main.run (thread ())

open Cmdliner

let user =
  let doc = "Username." in
  Arg.(required & opt (some string) None & info ["u"; "user"] ~doc)

let pass =
  let doc = "Password." in
  Arg.(required & opt (some string) None & info ["p"; "passwd"] ~doc)

let port =
  let doc = "Port." in
  Arg.(value & opt int 995 & info ["port"] ~doc)

let host =
  let doc = "Hostname." in
  Arg.(required & opt (some string) None & info ["h"; "host"] ~doc)

let number =
  let doc = "Number of emails." in
  Arg.(value & opt (some int) None & info ["n"; "number"] ~doc)

let cmd =
  let doc = "Verify MrMime with real-world emails." in
  let man =
  [ `S "Description"
  ; `P "$(tname) is test tool for MrMime." ] in
  Term.(pure do_cmd $ user $ pass $ port $ host $ number),
  Term.info "Message test" ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
