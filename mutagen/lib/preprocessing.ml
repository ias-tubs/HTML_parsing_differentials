open Containers

(** The ganss sanitizer wraps the result in a body tag, rewrap it as document fragment. *)
let ganss_cleanup parsed =
  let open Ast in
  match parsed with Tag ("body", [], _, children) -> Document_fragment children | t -> t

(** Some sanitizers wrap the result in a document, rewrap it as document_fragment *)
let document_cleanup parsed =
  let open Ast in
  match parsed with Document children -> Document_fragment children | t -> t

(** Some sanitizers wrap the result in a fragment tag, rewrap it as document_fragment *)
let fragment_cleanup parsed =
  let open Ast in
  match parsed with Tag ("fragment", [], _, children) -> Document_fragment children | t -> t

(** Some sanitizers wrap the result in a body tag (they call it body fragment) -> rewrap it as document_fragment *)
let body_fragment_cleanup parsed =
  let open Ast in
  match parsed with Tag ("body", [], _, children) -> Document_fragment children | t -> t

(** DOMPurify returns the whole HTML document, return just the contents of head and body *)
let dompurify_cleanup parsed =
  let open Ast in
  let flatten' node =
    match node with
    | Tag ("html", [], _, [ Tag ("head", [], _, ch); Tag ("body", [], _, cb) ]) -> ch @ cb
    | _ -> [ node ]
  in
  let dompurify_cleanup' tree =
    match tree with
    | Document children ->
        let children' = children |> List.map flatten' |> List.flatten in
        Document_fragment children'
    | Tag ("#document", [], _, children) ->
        let children' = children |> List.map flatten' |> List.flatten in
        Document_fragment children'
    | Tag (name, attrs, n, children) ->
        let children' = children |> List.map flatten' |> List.flatten in
        Tag (name, attrs, n, children')
    | _ -> tree
  in
  dompurify_cleanup' parsed

(** The Browsers return the whole HTML document, return just the contents of head and body *)
let browser_cleanup parsed =
  let open Ast in
  let tags_children = function Tag (_, _, _, cs) -> cs | x -> failwith @@ Fmt.str "'%a' is not a Tag\n%!" Ast.pp x in
  let in_document = function
    | Document children ->
        let html = children |> List.find (function Tag ("html", _, _, _) -> true | _ -> false) in
        html
    | x -> failwith @@ Fmt.str "'%a' is not a document!\n%!" Ast.pp x
  in
  let in_html = function
    | Tag ("html", _, _, children) ->
        let head = children |> List.find (function Tag ("head", _, _, _) -> true | _ -> false) in
        let body = children |> List.find (function Tag ("body", _, _, _) -> true | _ -> false) in
        let heads_children = tags_children head in
        let heads_children_num = List.length heads_children in
        let () =
          if heads_children_num > 0 then Fmt.epr "Head Tag has %d children: %a\n%!" heads_children_num Ast.pp head
          else ()
        in
        Document_fragment (tags_children body)
    | x -> failwith @@ Fmt.str "'%a' is not a html tag!\n%!" Ast.pp x
  in
  let clean = parsed |> in_document |> in_html in
  (*Fmt.pr "%a\n%!" Ast.pp clean;*)
  clean

let browser_postprocess =
  [ ("webkit", browser_cleanup); ("firefox", browser_cleanup); ("chromium", browser_cleanup) ] |> Hashtbl.of_list

let dompurify_preprocess (payload, ast) =
  if String.is_empty ast then
    let () = Fmt.pr "Empty ast for DOMPurify with payload '%s'\n%!" payload in
    let ast' = String.replace ~sub:"\"" ~by:"\\\"" payload in
    Fmt.str "(#text \"%s\")" ast'
  else ast

let sanitizer_preprocess =
  [
    ("dompurify (node)", dompurify_preprocess);
    ("dompurify (node20)", dompurify_preprocess);
    ("dompurify (lax, node)", dompurify_preprocess);
    ("dompurify (current, node22)", dompurify_preprocess);
    ("dompurify (lax, current, forceBody, node22)", dompurify_preprocess);
    ("dompurify (current, forceBody, node22)", dompurify_preprocess);
  ]
  |> Hashtbl.of_list

let sanitizer_postprocess =
  [
    ("dompurify (node)", dompurify_cleanup);
    ("dompurify (node20)", dompurify_cleanup);
    ("dompurify (lax, node)", dompurify_cleanup);
    ("dompurify (current, node22)", dompurify_cleanup);
    ("dompurify (lax, current, forceBody, node22)", dompurify_cleanup);
    ("dompurify (current, forceBody, node22)", dompurify_cleanup);
    ("ganss-fix-lax", ganss_cleanup);
    ("ganss-fix", ganss_cleanup);
    ("ganss-lax", ganss_cleanup);
    ("ganss", ganss_cleanup);
    ("html-rule", document_cleanup);
    ("html-rule-lax", document_cleanup);
    ("html-rule-fix", document_cleanup);
    ("html-rule-fix-lax", document_cleanup);
    ("caja2 (node)", fragment_cleanup);
    ("caja (node)", fragment_cleanup);
    ("sanitize-html (node)", fragment_cleanup);
    ("sanitize-html (curr, node)", fragment_cleanup);
    ("sanitize-html (curr, lax, node)", fragment_cleanup);
    ("jsoup", body_fragment_cleanup);
    ("jsoup-lax", body_fragment_cleanup);
    ("antisamy", CCFun.id);
    ("antisamy-anythinggoes", CCFun.id);
    ("antisamy-lax", CCFun.id);
    ("loofah-strip", CCFun.id);
    ("ruby-sanitize", CCFun.id);
    ("ruby-sanitize-lax", CCFun.id);
    ("ruby-sanitize-relaxed", CCFun.id);
  ]
  |> Hashtbl.of_list
