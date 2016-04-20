let address_list = (module Address.List : Alcotest.TESTABLE with type t = Address.List.t)

let make_compute_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> let _ = Address.List.of_string s in ())

let make_pp_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> let a = Address.List.of_string s in
   Printf.eprintf "%s\n%!" s;
   Printf.eprintf "%s\n%!" (Address.List.to_string a);
   Alcotest.(check address_list) "pp" a (Address.List.of_string @@ Address.List.to_string a))

exception Expect_exception
exception Invalid_exception

let make_exn_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () ->
   try ignore (Address.List.of_string s); raise Expect_exception
   with Invalid_argument s ->
        let p = "Address.List.of_string" in
        if (Bytes.sub s 0 (String.length p)) <> p
        then raise Invalid_exception)

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
  ; "\"\a\"@x.test"
  ; "\"\x07\"@x.test"
  ; "\"\\\x07\"@x.test"
  ; "\"\"@home.example"
  ; "jdoe@[RFC-5322-\\a-domain-literal]"
  ; "jdoe@[RFC-5322-\\t-domain-literal]"
  ; "jdoe@[RFC-5322-\\]-domain-literal]"
  ; "jdoe@[RFC-5322-domain-literal] (comment)"
  ; "pete@[255.255.255.255]"
  ; "\"mary\"@example.net"
  ; "\"\\\"\"@example.net"
  ; "\"john\".\"public\"@example.com"
  ; "\"mary\ smith\"@home.example"
  ; "\"mary\".smith@home.example"
  ; "\"mary\\\000\"@home.example"
  ; " richard @home.example"
  ; "richar@ home .example"
  ; "mary . smith@y.test"
  ; "\x0d\x0a jdoe@example.net"
  ; "\x0d\x0a \x0d\x0a jdoe@example.net"
  ; "(comment)smith@home.example"
  ; "(comment(comment))smith@home.example"
  ; "smith@(comment)home.example"
  ; "smith@(comment)[255.255.255.255]"
  ; "robert@xn--hxajbheg2az3al.xn--jxalpdlp"
  ; "xn--robert@x.test" ]

let err_tests =
  [ ""
  ; "mary"
  ; "@"
  ; "mary@"
  ; "@io"
  ; "@example.net"
  ; ".mary@example.net"
  ; "jdoe.@example.net"
  ; "pete..silly.test"
  ; "sm_i-th.com"
  ; "mary\@jdoe@one.test"
  ; "jdoe@.one.test"
  ; "jdon@one.test."
  ; "boss@nil..test"
  ; "\"\"\"@example.net"
  ; "\"\\\"@example.net"
  ; "jdoe\"@machine.example"
  ; "\"jdoe@machine.example"
  ; "\"john\"public@example.com"
  ; "john\"public\"@example.com"
  ; "\"john\"\"public\"@example.com"
  ; "\"mary\000\"@home.example"
  ; "pete@a[255.255.255.255]"
  ; "((comment)smith@home.example"
  ; "smith(coment)doe@home.example"
  ; "robert@henry.com\r"
  ; "(smith@home.example"
  ; "robert@[1.2.3.4"
  ; "\"john\\\"@example.com"
  ; "(comment\\)smith@home.example"
  ; "smith@home.example(comment\\)"
  ; "smith@home.example(comment\\"
  ; "robert@[RFC5322-[domain-literal\\]"
  ; "robert@[RFC5322-[domain-literal]"
  ; "robert@[RFC5322-[domain-literal\\"
  ; "marx@capitalism.ru\x0d"
  ; "\x0dmarx@capitalism.ru"
  ; "\"\x0dmarx\"@capitalism.ru"
  ; "(\x0d)marx@capitalism.ru"
  ; "marx@capitalism.ru(\x0d)"
  ; "smith@communism.uk\x0a"
  ; "\x0asmith@communism.uk"
  ; "\"\x0asmith\"@communism.uk"
  ; "(\x0a)smith@communism.uk"
  ; "smith@communism.uk(\x0a)" ]

let () =
  Alcotest.run "Address test"
  [ "compute", List.map make_compute_test tests
  ; "pp", List.map make_pp_test tests
  ; "exn", List.map make_exn_test err_tests ]
