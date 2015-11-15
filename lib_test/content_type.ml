module ContentType =
  struct
    include ContentType

    module Parser = struct
      include Parser

      (* for debugging *)
      type t = [%import: Parser.token] [@@deriving show]
    end

    let of_string str =
      let module Lexer = Lexer.Make(Sedlexing.Utf8) in
      let lexbuf = Lexer.from_string str in
      let parser =
        MenhirLib.Convert.Simplified.traditional2revised
          Parser.content_type_with_eof
      in
      parser (fun () -> Lexer.content_type lexbuf)
  end

let content_type =
  (module ContentType : Alcotest.TESTABLE with type t = ContentType.t)

let make_test str cty =
  Printf.sprintf "%S" str,
  `Slow,
  (fun () ->
    Alcotest.(check content_type) "equal"
      (ContentType.of_string str) cty)

let string_test =
  [
    make_test "text/plain; charset=us-ascii (Plain text)"
      (ContentType.make "text" "plain" ~parameters:[("charset", "us-ascii")]);
    make_test "text/plain; charset=\"us-ascii\""
      (ContentType.make "text" "plain" ~parameters:[("charset", "us-ascii")]);
    make_test "text/plain; charset=ISO-8859-1"
      (ContentType.make "text" "plain" ~parameters:[("charset", "ISO-8859-1")]);
  ]

let () =
  Alcotest.run "Content-Type test"
    [ "string", string_test ]
