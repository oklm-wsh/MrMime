let generate length =
  let gen () = match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let make_encode_bench size =
  let source = generate size in
  [ "encode",
    (fun s -> Base64.encode (Lexing.from_string s)),
    source
  ; "encode",
    (fun s -> B64.encode s),
    source ]

let make_decode_bench size =
  let result = B64.encode (generate size) in
  [ "decode",
    (fun s -> Base64.decode (Lexing.from_string s)),
    result
  ; "decode",
    (fun s -> B64.decode s),
    result ]

let encode_benchs =
  Array.init 9
    (fun idx -> make_encode_bench ((idx + 1) * 100))
  |> Array.to_list
  |> List.flatten

let decode_benchs =
  Array.init 9
    (fun idx -> make_decode_bench ((idx + 1) * 100))
  |> Array.to_list
  |> List.flatten

let () =
  Array.init 9
    (fun idx -> (idx + 1) * 100)
  |> Array.iter
     (fun n -> Benchmark.throughputN ~repeat:3 3 (make_encode_bench n)
               |> Benchmark.tabulate;
               Benchmark.throughputN ~repeat:3 3 (make_decode_bench n)
               |> Benchmark.tabulate)
