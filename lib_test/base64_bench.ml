module Base64_Latin1 = Base64.Make(Sedlexing.Latin1)
module Base64_Utf8   = Base64.Make(Sedlexing.Utf8)

module Utf16 =
struct
  include Sedlexing.Utf16

  let byte_order =
    if Sys.big_endian
    then Big_endian
    else Little_endian

  let from_stream stream =
    from_stream stream (Some byte_order)
  let from_string str =
    from_string str (Some byte_order)
  let from_channel ch =
    from_channel ch (Some byte_order)

  let lexeme lexbuf =
    lexeme lexbuf byte_order false

  let sub_lexeme lexbuf a b =
    sub_lexeme lexbuf a b byte_order false
end

module Base64_Utf16  = Base64.Make(Utf16)

let generate length =
  let gen () = match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let make_encode_bench size =
  let source = generate size in
  [ "latin1 encode",
    (fun s -> Base64_Latin1.encode (Sedlexing.Latin1.from_string s)),
    source
  ; "utf-8 encode",
    (fun s -> Base64_Utf8.encode (Sedlexing.Utf8.from_string s)),
    source
  (* TODO: fix this!
  ; "utf-16 encode",
    (fun s -> Base64_Utf16.encode (Utf16.from_string s)),
    source
  *)
  ; "encode",
    (fun s -> B64.encode s),
    source ]

let make_decode_bench size =
  let result = B64.encode (generate size) in
  [ "latin1 decode",
    (fun s -> Base64_Latin1.decode (Sedlexing.Latin1.from_string s)),
    result
  ; "utf-8 decode",
    (fun s -> Base64_Utf8.decode (Sedlexing.Utf8.from_string s)),
    result
  (* TODO: fix this!
  ; "utf-16 decode",
    (fun s -> Base64_Utf16.decode (Utf16.from_string s)),
    result
  *)
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
