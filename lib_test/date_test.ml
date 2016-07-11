module D =
struct
  type t = Date.date

  let pp = Date.pp
  let equal = Date.equal
end

let date = (module D : Alcotest.TESTABLE with type t = Date.date)

let make_compute_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> let _ = Date.of_string s in ())

let make_pp_test s =
  Printf.sprintf "%S" s,
  `Slow,
  (fun () -> match Date.of_string s with
   | None -> failwith "Invalid date"
   | Some v ->
     Alcotest.(check date) "pp" v (match Date.of_string @@ Date.to_string v with
                                   | None -> failwith "Invalid date"
                                   | Some v -> v))

let tests =
  [ "Fri, 21 Nov 1997 09:55:06 -0600"
  ; "Tue, 1 Jul 2003 10:52:37 +0200"
  ; "Thu, 13 Feb 1969 23:32:54 -0330"
  ; "Mon, 24 Nov 1997 14:22:01 -0800"
  ; "Thu,\r\n 13\r\n   Feb\r\n     1969\r\n 23:32\r\n          -0330 (Newfoundland Time)"
  ; "21 Nov 97 09:55:06 GMT"
  ; "Fri, 21 Nov 1997 09(comment):   55  :  06 -0600" ]


let () =
  Alcotest.run "Date test"
  [ "compute", List.map make_compute_test tests
  ; "pp", List.map make_pp_test tests ]
