module Version =
  struct
    include Version

    module Parser = struct
      include Parser

      (* for debugging *)
      type t = [%import: Parser.token] [@deriving show]
    end

    let of_string str =
      let lexbuf = Lexing.from_string str in
      Parser.version_with_eof Lexer.rfc2045_version lexbuf
  end

let version =
  (module Version : Alcotest.TESTABLE with type t = Version.t)

let make_test str vrs =
  Printf.sprintf "%S" str,
  `Slow,
  (fun () ->
    Alcotest.(check version) "equal"
      (Version.of_string str) vrs)

let string_test =
  [
    make_test "1.0" (Version.make 1 0);
    make_test "1.0 (produced by MetaSend Vx.x)" (Version.make 1 0);
    make_test "(produced by MetaSend Vx.x) 1.0" (Version.make 1 0);
    make_test "1.(produced by MetaSend Vx.x)0" (Version.make 1 0);
  ]

let () =
  Alcotest.run "Version test"
    [ "string", string_test ]
