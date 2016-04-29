(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env          = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()
let iana         = bool_of_string @@ BaseEnvLight.var_get "iana" env

let ppx_iana = "ppx/ppx_iana.byte"
let cmd_iana x = ppx_iana

let () = dispatch
  (function
   | After_hygiene ->
     if iana then dep ["ppx_iana"] ["iana.xml"];
     dep ["ppx_iana"] [ppx_iana];

     flag [ "ocaml"; "ocamldep"; "ppx_iana" ] (S [A "-ppx"; A (cmd_iana iana)]);
     flag [ "ocaml"; "compile"; "ppx_iana" ]  (S [A "-ppx"; A (cmd_iana iana)]);
     flag [ "ocaml"; "link"; "ppx_iana" ]     (S [A "-ppx"; A (cmd_iana iana)]);

     dispatch_default After_hygiene
   | x -> dispatch_default x)
