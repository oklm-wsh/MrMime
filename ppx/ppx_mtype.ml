open Ast_mapper
open Ast_helper
open Asttypes
open Ast_convenience
open Parsetree
open Longident
open Cmdliner

module Map = Map.Make(String)
module Set = Set.Make(String)

type t =
  | Node of (Xmlm.pos * Xmlm.tag * t list)
  | Leaf of (Xmlm.pos * string)

let is_tag name ((_, tag), _) = name = tag
let has_tag name (_, childs) =
  List.exists (function Node (_, tag, _) -> is_tag name tag | _ -> false) childs
let has_attribute name (_, attrs) =
  List.exists (fun ((_, attribute), _) -> name = attribute) attrs
let get_attribute name (_, attrs) =
  List.find (fun ((_, attribute), _) -> name = attribute) attrs
  |> fun ((_, _), value) -> value
let get_tag name (_, childs) =
  List.find (function Node (_, tag, _) -> is_tag name tag | _ -> false) childs
  |> function Node (_, tag, childs) -> (tag, childs) | _ -> assert false
let get_leaf (_, childs) =
  List.find (function Leaf _ -> true | _ -> false) childs
  |> function Leaf (_, data) -> data | _ -> assert false

let t_of_xml input =
  let el tag datas = Node (Xmlm.pos input, tag, datas) in
  let data data = Leaf (Xmlm.pos input, data) in
  Xmlm.input_doc_tree ~el ~data input

let make xml =
  let make_subty childs =
    let rec aux acc = function
      | Node (_, tag, childs) :: r
        when is_tag "record" tag && has_tag "name" (tag, childs) ->
        let subty = get_leaf @@ get_tag "name" (tag, childs) in
        aux (Set.add subty acc) r
      | x :: r -> aux acc r
      | [] -> acc
    in

    aux Set.empty childs
  in
  let make_ty childs =
    let rec aux acc = function
      | Node (_, tag, childs) :: r
        when is_tag "registry" tag && has_attribute "id" tag ->
        let ty  = get_attribute "id" tag in
        let set = make_subty childs in
        aux (Map.add ty set acc) r
      | x :: r -> aux acc r
      | [] -> acc
    in

    aux Map.empty childs
  in

  match xml with
  | (_, Node (_, tag, childs)) when is_tag "registry" tag -> make_ty childs
  | _ -> Map.empty

let sp = Printf.sprintf

let compute filename =
  try
    let ch    = open_in filename in
    let input = Xmlm.make_input (`Channel ch) in
    make @@ t_of_xml input
  with exn ->
    raise (Invalid_argument (sp "Invalid XML file: %s" (Printexc.to_string exn)))

let make_expr symbol db =
  Map.fold (fun key value acc ->
    let to_expr_lst set =
      let lst = Set.fold (fun x r -> (str x) :: r) set [] in
      List.fold_left (fun x acc -> [%expr [%e acc] :: [%e x]]) (nil ()) lst
    in
    [%expr Map.add [%e (str key)] (Set.of_list [%e (to_expr_lst value)]) [%e acc]]) db symbol

let ppx =
  { default_mapper with
    expr = fun mapper expr ->
    match expr with
    | { pexp_desc =
        Pexp_extension ({ txt = "mtype"; _ }, pstr);
        pexp_loc; _ } ->
      begin match pstr with
      | PStr [{ pstr_desc =
                Pstr_eval ({ pexp_loc = loc;
                             pexp_desc = Pexp_apply (database, [Nolabel, { pexp_desc = Pexp_constant (Pconst_string (filename, _)) } ]) }, _) }]
        ->
        let db = compute filename in
        make_expr database db
      | _ -> default_mapper.expr mapper expr end
    | x -> default_mapper.expr mapper x }

let cmd =
  let doc = "PPX to fill a Media types IANA database" in
  let man =
  [ `S "Description"
  ; `P "$(tname) is a ppx to fill a database from a Media types IANA XML file."
  ; `S "Requirement"
  ; `P "The PPX can only refill a: "
  ; `I ("ocaml", "Set.Make(String).t Map.Make(String).t")
  ; `P "In other case, the compilation will fail." ]
  in
  Term.(pure ppx), Term.info "ppx_mtype" ~doc ~man

let iana_mapper argv =
  match Term.eval ~argv:(Array.of_list ("ppx_mtype" :: argv)) cmd with
  | `Error _   -> exit 1
  | `Ok mapper -> mapper
  | _          -> exit 0

let () = register "mtype" iana_mapper
