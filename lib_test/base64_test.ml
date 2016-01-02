let () = Random.self_init ()

let print_ellipsis () str =
  if String.length str > 20
  then Printf.sprintf "%s… (%d)" (String.sub str 0 20) (String.length str)
  else Printf.sprintf "%s" str

let generate length =
  let gen () = match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let make_identity_test length =
  let str = generate length in
  Printf.sprintf "%a" print_ellipsis str,
  `Slow,
  (fun () ->
    Alcotest.(check string) "identity"
      (Base64.Encode.string_of_string str
       |> Base64.Decode.string_of_string)
      str)

let identity_test size =
  Array.init size
    (fun _ -> make_identity_test (40 + Random.int 120))
  |> Array.to_list

let () =
  Alcotest.run "Base64 test"
    [ "identity", identity_test 100 ]
