(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () = flag ["js_of_ocaml"] (A "+weak.js")

let cstruct =
  try let pkg = Ocamlbuild_pack.Findlib.query "cstruct" in
      flag ["js_of_ocaml"] (A (Filename.concat pkg.Ocamlbuild_pack.Findlib.location "cstruct.js"))
  with exn -> ()

let () = dispatch
  (fun hook ->
    dispatch_default hook;
    Ocamlbuild_js_of_ocaml.dispatcher
      ~oasis_executables:[ "web/validator.byte" ] hook)
