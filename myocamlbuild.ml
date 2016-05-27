(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env          = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()
let mtype        = bool_of_string @@ BaseEnvLight.var_get "mtype" env
let tag          = bool_of_string @@ BaseEnvLight.var_get "tag" env
let update       = bool_of_string @@ BaseEnvLight.var_get "update" env
let trace        = bool_of_string @@ BaseEnvLight.var_get "trace" env

let ppx_mtype = "ppx/ppx_mtype.byte"
let get_mtype = "ppx/get_mtype.byte"
let cmd_mtype = ppx_mtype

let ppx_tag = "ppx/ppx_tag.byte"
let get_tag = "ppx/get_tag.byte"
let cmd_tag = ppx_tag

let ppx_debug = "ppx/ppx_debug.byte"
let opt_debug = function true -> "-debug" | false -> "-no-debug"
let cmd_debug x = ppx_debug ^ " " ^ (opt_debug x)

let logs = S [ A "-package"; A "logs";
               A "-package"; A "logs.cli";
               A "-package"; A "logs.fmt" ]

let () =
  rule "media type"
    ~prod:"mtype.xml"
    ~dep:get_mtype
    (fun env build -> Cmd (S [A get_mtype]))

let () =
  rule "tag"
    ~prod:"tag.xml"
    ~dep:get_tag
    (fun env build -> Cmd (S [A get_tag]))

let () = dispatch
  (function
   | After_hygiene ->
     dep ["ppx_mtype"] ["mtype.xml"];
     dep ["ppx_mtype"] [ppx_mtype];
     dep ["ppx_debug"] [ppx_debug];

     flag [ "ocaml"; "ocamldep"; "ppx_mtype" ] (S [A "-ppx"; A cmd_mtype]);
     flag [ "ocaml"; "compile"; "ppx_mtype" ]  (S [A "-ppx"; A cmd_mtype]);
     flag [ "ocaml"; "link"; "ppx_mtype" ]     (S [A "-ppx"; A cmd_mtype]);

     dep ["ppx_tag"] ["tag.xml"];
     dep ["ppx_tag"] [ppx_tag];

     flag [ "ocaml"; "ocamldep"; "ppx_tag" ] (S [A "-ppx"; A cmd_tag]);
     flag [ "ocaml"; "compile"; "ppx_tag" ]  (S [A "-ppx"; A cmd_tag]);
     flag [ "ocaml"; "link"; "ppx_tag" ]     (S [A "-ppx"; A cmd_tag]);

     if trace
     then begin
       flag_and_dep [ "ocaml"; "ocamldep"; "ppx_debug" ]  logs;
       flag_and_dep [ "ocaml"; "compile";  "ppx_debug" ]  logs;
       flag_and_dep [ "ocaml"; "ocamldep"; "use_mrmime" ] logs;
       flag_and_dep [ "ocaml"; "compile";  "use_mrmime" ] logs;
       flag_and_dep [ "ocaml"; "link";     "use_mrmime" ] logs;
     end;

     flag [ "ocaml"; "ocamldep"; "ppx_debug" ] (S [ A "-ppx"; A (cmd_debug trace) ]);
     flag [ "ocaml"; "compile";  "ppx_debug" ] (S [ A "-ppx"; A (cmd_debug trace) ]);

     dispatch_default After_hygiene
   | x -> dispatch_default x)
