let address_list = (module Address.List : Alcotest.TESTABLE with type t = Address.List.t)

let make_compute_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> let _ = Address.of_string in ())

let make_pp_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> let a = Address.List.of_string s in
   Printf.eprintf "%s\n%!" s;
   Printf.eprintf "%s\n%!" (Address.List.to_string a);
   Alcotest.(check address_list) "pp" a (Address.List.of_string @@ Address.List.to_string a))

let tests =
  [ "Mary Smith <mary@example.net>"
  ; "Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>"
  ; "A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;"
  ; "Undisclosed recipients:;"
  ; "\"Mary Smith: Personal Account\" <smith@home.example>"
  ; "John Doe <jdoe@machine.example>"
  ; "Pete(A nice \) chap) <pete(his account)@silly.test(his host)>"
  ; "A Group(Some people)\r\n    :Chris Jones <c@(Chris's host.)public.example>,\r\n      joe@example.org,\r\n  John <jdoe@one.test> (my dear friend); (the end of the group)"
  ; "(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;"
  ; "Mary Smith <@node.test:mary@example.net>, , jdoe@test  . example"
  ; "Joe Q. Public <john.q.public@example.com>"
  ; "John Doe <jdoe@machine(comment).  example>"
  ; "Mary Smith\r\n  \r\n  <mary@example.net>"
  ; "<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>"
  ; "!#$%&`*+/=?^`{|}~@iana.org"
  ; "(\x07;)mary@example.net"
  ; "\"\\\x0a\"@x.test"
  ; "\"\x07\"@x.test"
  ; "\"\\\x07\"@x.test"
  ; "\"\"@home.example"
  ; "jdoe@[RFC-5322-\\a-domain-literal]"
  ; "jdoe@[RFC-5322-\\t-domain-literal]"
  ; "jdoe@[RFC-5322-\\]-domain-literal]"
  ]


let () =
  Alcotest.run "Address test"
  [ "compute", List.map make_compute_test tests
  ; "pp", List.map make_pp_test tests ]
