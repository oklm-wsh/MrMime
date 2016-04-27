(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () = flag ["js_of_ocaml"] (A "+weak.js")

let cstruct = Ocamlbuild_pack.Findlib.query "cstruct"

let () = flag ["js_of_ocaml"] (A (Filename.concat cstruct.Ocamlbuild_pack.Findlib.location "cstruct.js"))

let () = dispatch
  (fun hook ->
    dispatch_default hook;
    Ocamlbuild_js_of_ocaml.dispatcher
      ~oasis_executables:[ "web/validator.byte" ] hook)
