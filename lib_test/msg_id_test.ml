let () = Printexc.record_backtrace true

let msg_id =
  (module MsgID : Alcotest.TESTABLE with type t = MsgID.t)

let make_test str i =
  Printf.sprintf "%S" str,
  `Slow,
  (fun () ->
    Alcotest.(check msg_id) "equal"
      (MsgID.of_string str) i)

let string_test =
  [ "<1234@local.machine.example>",                MsgID.make "1234" "local.machine.example"
  ; "<5678.21-Nov-1997@example.com>",              MsgID.make "5678.21-Nov-1997" "example.com"
  ; "<testabcd.1234@silly.example>",               MsgID.make "testabcd.1234" "silly.example"
  ; "<3456@example.net>",                          MsgID.make "3456" "example.net"
  ; "<abcd.1234@local.machine.tld>",               MsgID.make "abcd.1234" "local.machine.tld"
  ; "<78910@example.net>",                         MsgID.make "78910" "example.net"
  ; "             <testabcd.1234@silly.test>",     MsgID.make "testabcd.1234" "silly.test"
  ; "<1234   @   local(blah)  .machine .example>", MsgID.make "1234" "local.machine.example"
  ] |> List.map (fun (str, i) -> make_test str i)

let () =
  Alcotest.run "Message ID test"
    [ "string", string_test ]
