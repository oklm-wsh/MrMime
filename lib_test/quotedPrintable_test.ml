module Client = Cohttp_lwt_unix.Client
module Body   = Cohttp_lwt_body

open Lwt.Infix

let () = Random.self_init ()
let () = Printexc.record_backtrace true

module List =
struct
  include List

  let make size value =
    let rec aux = function
      | 0 -> []
      | n when n > 0 -> value :: aux (n - 1)
      | _ -> raise (Invalid_argument "List.make")
    in aux size
end

type size =
  | Short
  | Medium
  | Long
  | VeryLong

let size_to_string = function
  | Short -> "short"
  | Medium -> "medium"
  | Long -> "long"
  | VeryLong -> "verylong"

let size_of_int = function
  | 0 -> Short
  | 1 -> Medium
  | 2 -> Long
  | 3 -> VeryLong
  | _ -> raise (Invalid_argument "size_of_int")

let lorem_ipsum ?(size = Medium) number =
  let ( / ) = Filename.concat in
  let api  = Uri.of_string "http://loripsum.net/" in
  let path = "api" / (string_of_int number) / (size_to_string size) / "plaintext" in
  Client.get (Uri.with_path api path) >>= fun (_, body) -> Body.to_string body

let print_ellipsis () str =
  if String.length str > 20
  then Printf.sprintf "%s… (%d)" (String.sub str 0 20) (String.length str)
  else Printf.sprintf "%s" str

let make_identity_test () =
  let size = Random.int 3 |> size_of_int in
  let number = Random.int 10 + 1 in
  lorem_ipsum ~size number
  >>= fun str -> Lwt_unix.sleep 2.0
  >>= fun () ->
  (Printf.sprintf "%a" print_ellipsis str,
   `Slow,
   (fun () ->
     let buf = Buffer.create 16 in
     let out = Buffer.create 16 in
     Printf.printf "begin of test\n%!";
     Printf.printf "str:\n%s%!\n" str;

     QuotedPrintable.encode buf (Lexing.from_string str);
     Printf.printf "buf:\n%s%!\n" (Buffer.contents buf);

     QuotedPrintable.decode out (Lexing.from_string (Buffer.contents buf));
     Printf.printf "out:\n%s%!\n" (Buffer.contents out);

     Alcotest.(check string) "identity" str (Buffer.contents out)))
  |> Lwt.return

let main () =
  Lwt_list.map_s make_identity_test (List.make 5 ())
  >|= fun l -> Alcotest.run "Quoted-Printable test" [ "identity", l ]

let () = Lwt_unix.run (main ())
