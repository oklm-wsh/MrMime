module UnstrictHeader =
struct
  type t = Header.unstrict

  let pp = Header.pp
  let equal : t -> t -> bool = Header.equal
end

let header = (module UnstrictHeader : Alcotest.TESTABLE with type t = UnstrictHeader.t)

let make_compute_test s =
  Printf.sprintf "header",
  `Slow,
  (fun () -> let _ = Header.of_string s in ())

let make_pp_test s =
  Printf.sprintf "header",
  `Slow,
  (fun () -> let a = Header.of_string s in
   Format.eprintf "%s\n----------\n%!" s;
   Format.eprintf "%s\n----------\n%!" (Header.to_string a);
   Alcotest.(check header) "pp" a (Header.of_string @@ Header.to_string a))

let tests =
  [
(* See RFC 5322 § Appendix A.1.1 *)
{|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>
|};

(* See RFC 5322 § Appendix A.1.2 *)
{|From: "Joe Q. Public" <john.q.public@example.com>
To: Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>
Cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>
|};

(* See RFC 5322 § Appendix A.1.3 *)
{|From: Pete <pete@silly.example>
To: A Group:Ed Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;
Cc: Undisclosed recipients:;
Date: Thu, 13 Feb 1969 23:32:54 -0330
Message-ID: <testabcd.1234@silly.example>
|};

(* See RFC 5322 § Appendix A.2 *)
{|From: Mary Smith <mary@example.net>
To: John Doe <jdoe@machine.example>
Reply-To: "Mary Smith: Personal Account" <smith@home.example>
Subject: Re: Saying Hello
Date: Fri, 21 Nov 1997 10:01:10 -0600
Message-ID: <3456@example.net>
In-Reply-To: <1234@local.machine.example>
References: <1234@local.machine.example>
|};

(* See RFC 5322 § Appendix A.3 *)
{|Resent-From: Mary Smith <mary@example.net>
Resent-To: Jane Brown <j-brown@other.example>
Resent-Date: Mon, 24 Nov 1997 14:22:01 -0800
Resent-Message-ID: <78910@example.net>
From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.machine.example>
|};

(* See RFC 5322 § Appendix A.4 *)
{|Received: from x.y.test
   by example.net
   via TCP
   with ESMTP
   id ABC12345
   for <mary@example.net>;  21 Nov 1997 10:05:43 -0600
Received: from node.example by x.y.test; 21 Nov 1997 10:01:22 -0600
From: John Doe <jdoe@node.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: Fri, 21 Nov 1997 09:55:06 -0600
Message-ID: <1234@local.node.example>
|};

(* See RFC 5322 § Appendix A.5 *)
{|From: Pete(A nice \) chap) <pete(his account)@silly.test(his host)>
To:A Group(Some people)
     :Chris Jones <c@(Chris's host.)public.example>,
         joe@example.org,
  John <jdoe@one.test> (my dear friend); (the end of the group)
Cc:(Empty list)(start)Hidden recipients  :(nobody(that I know))  ;
Date: Thu,
      13
        Feb
          1969
      23:32
               -0330 (Newfoundland Time)
Message-ID:              <testabcd.1234@silly.test>
|};

(* See RFC 5322 § Appendix A.6.1 *)
{|From: Joe Q. Public <john.q.public@example.com>
To: Mary Smith <@node.test:mary@example.net>, , jdoe@test  . example
Date: Tue, 1 Jul 2003 10:52:37 +0200
Message-ID: <5678.21-Nov-1997@example.com>
|};

(* See RFC 5322 § Appendix A.6.2 *)
{|From: John Doe <jdoe@machine.example>
To: Mary Smith <mary@example.net>
Subject: Saying Hello
Date: 21 Nov 97 09:55:06 GMT
Message-ID: <1234@local.machine.example>
|};

(* See RFC 5322 § Appendix A.6.3 *)
{|From  : John Doe <jdoe@machine(comment).  example>
To    : Mary Smith
  
          <mary@example.net>
Subject     : Saying Hello
Date  : Fri, 21 Nov 1997 09(comment):   55  :  06 -0600
Message-ID  : <1234   @   local(blah)  .machine .example>
|};

(* See RFC 822 § A.3.1 *)
{|Date:     26 Aug 76 14:29 EDT
From:     Jones@Registry.Org
Bcc:
|};

(* See RFC 822 § A.3.2 *)
{|Date:     26 Aug 76 14:30 EDT
From:     George Jones<Group@Host>
Sender:   Secy@SHOST
To:       "Al Neuman"@Mad-Host,
          Sam.Irving@Other-Host
Message-ID:  <some.string@SHOST>
|};

(* See RFC 822 § A.3.3 *)
{|Date     :  27 Aug 76 09:32 PDT
From     :  Ken Davis <KDavis@This-Host.This-net>
Subject  :  Re: The Syntax in the RFC
Sender   :  KSecy@Other-Host
Reply-To :  Sam.Irving@Reg.Organization
To       :  George Jones <Group@Some-Reg.An-Org>,
            Al.Neuman@MAD.Publisher
cc       :  Important folk:
              Tom Softwood <Balsa@Tree.Root>,
              "Sam Irving"@Other-Host;,
            Standard Distribution:
              /main/davis/people/standard@Other-Host,
              "<Jones>standard.dist.3"@Tops-20-Host>;
Comment  : Sam is away on business. He asked me to handle
           his mail for him.  He'll be able to provide  a
           more  accurate  explanation  when  he  returns
           next week.
In-Reply-To: <some.string@DBM.Group>, George's message
X-Special-action:  This is a sample of user-defined field-
            names.  There could also be a field-name
            "Special-action", but its name might later be
            preempted
Message-ID: <4231.629.XYzi-What@Other-Host>
|};

(* See RFC 2047 § 8 *)
{|From: =?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>
Date     :  27 Aug 76 09:32 PDT
To: =?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?= <keld@dkuug.dk>
CC: =?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>
Subject: =?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=
 =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=
|};
  ]

let () =
  Alcotest.run "Header test"
  [ "compute", List.map make_compute_test tests
  ; "pp", List.map make_pp_test tests ]
