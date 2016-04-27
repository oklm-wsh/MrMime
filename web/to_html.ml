open Html5
open Cmdliner

let zenburn_css  = "http://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/rainbow.min.css"
let highligh_js  = "http://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js"

let script = function
  | None -> M.html (M.head (M.title (M.pcdata "Validator")) []) (M.body [])
  | Some script ->
    M.html
      (M.head
       (M.title (M.pcdata "Validator"))
       [ M.meta ~a:[ M.a_http_equiv "content-type"
                   ; M.a_content "text/html; charset=utf-8"] ()
       ; M.link ~rel:[`Stylesheet]
                ~href:(Xml.uri_of_string "web/style.css")
                ~a:[M.a_mime_type "text/css"] ()
       ; M.script ~a:[ M.a_src @@ Xml.uri_of_string highligh_js ] @@ M.pcdata ""
       ; M.link ~rel:[`Stylesheet]
                ~href:(Xml.uri_of_string zenburn_css)
                ~a:[M.a_mime_type "text/css"] ()])
      (M.body ~a:[ M.a_id "main" ]
       [ M.div ~a:[ M.a_class ["field"]; M.a_id "validate" ]
         [ M.input ~a:[ M.a_input_type `Text
                      ; M.a_id "validate-term"
                      ; M.a_placeholder "the hell email" ] ()
         ; M.button ~a:[ M.a_id "validate-button" ]
           [ M.pcdata "Check!" ]]
       ; M.script ~a:[ M.a_src @@ Xml.uri_of_string script ] @@ M.pcdata "" ])

let main s =
  let p = print_string in
  let c = script s in
  P.print ~output:p c;
  `Ok ()

let script =
  let doc = "Script JS" in
  Arg.(value & opt (some string) None & info ["s"; "script"] ~docv:"script" ~doc)

let cmd =
  let doc = "Rendering validator page" in
  let man =
  [ `P "BUGS"
  ; `S "Email them to <romain.calascibetta@gmail.com>" ]
  in Term.(ret (pure main $ script)), Term.info "to_html" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _        -> exit 0
