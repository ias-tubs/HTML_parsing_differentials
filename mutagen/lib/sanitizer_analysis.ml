open Containers
module L = Util.Lookups
module N = Ast.Namespace

module Encodings = struct
  type t = Unencoded | XML_encoded | URI_encoded | Several of t list
  [@@deriving show { with_path = false }, hash, eq, yojson]

  (** Is the sample encoded (either with URI or XML encoding)? *)
  let determine_encoding (pretty : string) =
    let is_xml_encoded = String.mem ~sub:"Xml_Encode" pretty in
    let is_uri_encoded = String.mem ~sub:"EncodeURI" pretty in
    match (is_xml_encoded, is_uri_encoded) with
    | false, false -> Unencoded
    | false, true -> URI_encoded
    | true, false -> XML_encoded
    | true, true -> Several [ XML_encoded; URI_encoded ]

end

let img_payload = Gen.Payload.print Gen.Payload.Img_tag
let image_payload = Gen.Payload.print Gen.Payload.Image_tag
let script_payload = Gen.Payload.print Gen.Payload.Script_tag
(** Does [text] contain one of the generated payloads? *)
let payload_survived text =
  String.mem ~sub:img_payload text  || String.mem ~sub:script_payload text || String.mem ~sub:image_payload text

let get_path_to_payload ?(verbose = false) ast =
  let select_path paths node =
    let paths' = paths |> List.find_all (fun p -> List.length p > 0) in
    match List.length paths' with
    | 0 -> []
    | 1 -> List.hd paths'
    | n ->
      if verbose then
        Fmt.pr "Found multiple (i.e., %d) child paths in %a\n%!" n Ast.pp node
else ();
        List.hd paths'
  in
  let rec get_path_to_payload' acc ast =
    match ast with
    | Ast.Document_type _ -> []
    | (Ast.Document_fragment cs | Ast.Document cs) as t ->
        let paths = cs |> List.map (get_path_to_payload' (t :: acc)) in
        select_path paths t
    | Ast.Tag (_, _, _, cs) as t ->
        if Ast.is_payload t then t :: acc
        else
          let paths = cs |> List.map (get_path_to_payload' (t :: acc)) in
          select_path paths t
    | (Ast.Comment _ | Ast.Cdata _ | Ast.Text _) as t -> if Ast.is_payload t then t :: acc else []
    (*| _ -> failwith "foo"*)
  in
  get_path_to_payload' [] ast |> List.rev

module Parsing_issues = struct
  type t =
    | Wrong_cdata of (int * Ast.t)
    | Comment_parsing_not_detecting_bang of (int * Ast.t)
    | Surviving_payload_in_text_node of (int * Ast.t)
        (** This is not really a parsing issue and more of a sanitizer issue, consider to move around *)
    | Text_tag_with_non_text_content of (string * int * Ast.t)
        (** This is not really a parsing issue and more of a sanitizer issue, consider to move around *)
    | Does_not_detect_end_of_tag of (string * int * Ast.t)
        (** E.g., id: 241403 for noscript or id: 68339 for noembed *)
    | Decodes_text_node of (Encodings.t * int * Ast.t)
    | Decodes_attribute_node of (Encodings.t * int * Ast.t)
    | Decodes_comment_node of (Encodings.t * int * Ast.t)
    (** *)
    | Allows_to_close_plaintext of (int * Ast.t)
    (** *)
    | Failure_to_break_foreign of (int * Ast.t * Ast.t)
  [@@deriving show { with_path = false }, eq, hash]

  let get_id = function
    | Wrong_cdata (id,_)
    | Comment_parsing_not_detecting_bang(id, _)
    | Surviving_payload_in_text_node(id, _) 
    | Text_tag_with_non_text_content(_,id,_) 
    | Does_not_detect_end_of_tag(_,id,_)
    | Decodes_text_node(_,id,_)
    | Decodes_attribute_node(_,id,_)
    | Decodes_comment_node(_,id,_)
    | Allows_to_close_plaintext(id, _)
    | Failure_to_break_foreign(id, _,_) -> id

  let pp_verbose fmt = function
    | Wrong_cdata (id, _) -> Fmt.pf fmt "Wrong CDATA handling (%d)" id
    | Comment_parsing_not_detecting_bang (id, _) -> Fmt.pf fmt "Does not detect bang closing comments (%d)" id
    | Surviving_payload_in_text_node (id, _) -> Fmt.pf fmt "Does not escape texts node (%d)" id
    | Does_not_detect_end_of_tag (name, id, _) -> Fmt.pf fmt "Does not detect the end of %s (%d)" name id
    | Text_tag_with_non_text_content (name, id, _) -> Fmt.pf fmt "Has %s tags with non text content (%d)" name id
    | Decodes_text_node (_, id, _) -> Fmt.pf fmt "Decodes text node (%d)" id
    | Decodes_attribute_node (_, id, _) -> Fmt.pf fmt "Decodes attribute node (%d)" id
    | Decodes_comment_node (_, id, _) -> Fmt.pf fmt "Decodes comment node (%d)" id
    | Allows_to_close_plaintext ( id, _) -> Fmt.pf fmt "Allows to close plaintext nodes (%d)" id
    | Failure_to_break_foreign ( id, _, _) -> Fmt.pf fmt "Does not break foreign content (%d)" id

  let show = function
    | Wrong_cdata _ -> "Wrong CDATA handling"
    | Comment_parsing_not_detecting_bang _ -> "Does not detect bang closing comments"
    | Surviving_payload_in_text_node _ -> "Does not escape text nodes"
    | Does_not_detect_end_of_tag (name, _, _) -> Fmt.str "Does not detect end of %s" name
    | Text_tag_with_non_text_content (name, _, _) -> Fmt.str "%s with non text content" name
    | Decodes_text_node (_, _, _) -> "Decodes text node"
    | Decodes_attribute_node (_, _, _) -> "Decodes attribute node"
    | Decodes_comment_node (_, id, _) -> "Decodes comment node"
    | Allows_to_close_plaintext (_, _) -> "Allows to close plaintext nodes"
    | Failure_to_break_foreign (_,_,_) -> "Does not break foreign content"

  let pp fmt = function
    | Wrong_cdata _ -> Fmt.pf fmt "%s" "Wrong CDATA handling"
    | Comment_parsing_not_detecting_bang _ -> Fmt.pf fmt "%s" "Does not detect bang closing comments"
    | Surviving_payload_in_text_node _ -> Fmt.pf fmt "%s" "Does not escape payload in text node"
    | Does_not_detect_end_of_tag (name, _, _) -> Fmt.pf fmt "Does not detect end of %s" name
    | Text_tag_with_non_text_content (name, _, _) -> Fmt.pf fmt "%s with non text content" name
    | Decodes_text_node (_, _, _) -> Fmt.pf fmt "%s" "Decodes text node"
    | Decodes_attribute_node (_, _, _) -> Fmt.pf fmt "%s" "Decodes attribute node"
    | Decodes_comment_node (_, id, _) -> Fmt.pf fmt "%s" "Decodes comment node"
    | Allows_to_close_plaintext (_, _) -> Fmt.pf fmt "%s" "Allows to close plaintext nodes"
    | Failure_to_break_foreign (_,_,_) -> Fmt.pf fmt "%s" "Does not break foreign content"
end

module Processing_error = struct
  type t = Test_error | Parsing_error of string * int [@@deriving show { with_path = false }, eq]

  let fmt pr = function
    | Test_error -> Fmt.pf pr "Test error"
    | Parsing_error (input, id) -> Fmt.pf pr "Parsing error (%d) for: '%s'" id input
end

module ParsedHt = CCHashtbl.Make (struct
  type t = int * int

  let equal (lid, ls) (rid, rs) = Int.equal lid rid && Int.equal ls rs
  let hash (id, s) = Hash.pair Hash.int Hash.int (id, s)
end)
module ReferenceHt = CCHashtbl.Make (struct
  type t = int * int * int

  let equal (lid, lb, lm) (rid, rb, rm) = Int.equal lid rid && Int.equal lb rb && Int.equal lm rm
  let hash (id, b, m) = (Hash.triple Hash.int Hash.int Hash.int) (id, b, m)
end)

let make_reference_table data =
  let tbl = ReferenceHt.create 10000 in
  data |> List.iter (fun (gen_id, browser_id, mode_id, payload, pretty, dom, result, exec) ->
      ReferenceHt.add tbl (gen_id, browser_id, mode_id) (payload, pretty, dom, result, exec));
  tbl

type issue_ht = (string, Parsing_issues.t Bounded_list.t) Hashtbl.t
type t = {
    errors : (string, Processing_error.t Bounded_list.t) Hashtbl.t;
    issues: (string, issue_ht) Hashtbl.t;
}
let setup sanitizers =
  let errors = sanitizers |> List.map (fun s -> (s, Bounded_list.create 5)) |> Hashtbl.of_list in
  let issues = sanitizers |> List.map (fun s -> (s, Hashtbl.create 5)) |> Hashtbl.of_list in
  { errors ; issues }

let add_parsing_issue ctx sanitizer_name issue =
  let entry = Hashtbl.get ctx.issues sanitizer_name in
  match entry with
  | Some e ->
    let k = Parsing_issues.show issue in
    let cnts = Hashtbl.get_or ~default:(Bounded_list.create 3) e k in 
    Bounded_list.add cnts issue;
    Hashtbl.replace e k cnts
  | None -> failwith @@ Fmt.str "Sanitizer '%s' is unknown!\n%!" sanitizer_name

let add_issue ctx sanitizer_name issue =
  let entry = Hashtbl.get ctx.errors sanitizer_name in
  match entry with
  | Some e -> Bounded_list.add e issue
  | None -> failwith @@ Fmt.str "Sanitizer '%s' is unknown!\n%!" sanitizer_name

let comment_containing_bang sanitizer_name ast =
  match (ast, sanitizer_name) with
  | Ast.Comment text, "typo3"
  | Ast.Comment text, "typo3-lax"
  | Ast.Comment text, "typo3-fix"
  | Ast.Comment text, "typo3-fix-lax"
  | Ast.Comment text, "typo3-fix2"
  | Ast.Comment text, "typo3-fix2-lax" ->
      let cdata = String.starts_with ~prefix:"<![CDATA[" text in
      let bang_idx = String.find ~sub:"--!>" text in
      let payload_idx = String.find ~sub:"mxss" text in
      let contains = (not cdata) && bang_idx > -1 && bang_idx < payload_idx in
      let _ = sanitizer_name in
      (*Fmt.pr "%s: does %s contain '--!>'? %b (%d)\n%!" sanitizer_name text contains bang_idx;*)
      contains
  | Ast.Comment text, _ ->
      let bang_idx = String.find ~sub:"--!>" text in
      let payload_idx = String.find ~sub:"mxss" text in
      let contains = bang_idx > -1 && bang_idx < payload_idx in
      let _ = sanitizer_name in
      (*Fmt.pr "%s: does %s contain '--!>'? %b (%d)\n%!" sanitizer_name text contains bang_idx;*)
      contains
  | _ -> false

let detect_wrong_cdata _ id sanitizer_name _ node _ _ context = 
  match context with
  | Ast.Payload_context.In_comment ->
      let text = (begin match node with | Ast.Comment(c) -> c | _ -> failwith @@ Fmt.str "Should be in comment (%a) but AST ('%a') does not match\n%!" Ast.Payload_context.pp context Ast.pp node end) in
      let cdata = String.starts_with ~prefix:"<![CDATA[" text in
      let typo3 = String.starts_with ~prefix:"typo3" sanitizer_name in
      if cdata && typo3 then
        Some [Parsing_issues.Wrong_cdata (id, node)]
      else None
  | Ast.Payload_context.In_cdata -> Some [Parsing_issues.Wrong_cdata (id, node)]
  | _ -> None

let detect_survived_in_text _ id _ _ node survived _ = function
  | Ast.Payload_context.In_text ->
    if survived then Some [Parsing_issues.Surviving_payload_in_text_node (id, node)] else None
  | _ -> None

let detect_decoded_in_comment _ id _ _ node survived encoding = function
  | Ast.Payload_context.In_comment -> (
      match (survived, encoding) with
      | _, Some Encodings.Unencoded -> None
      | false, _ -> None
      | true, None -> failwith "Can't happen"
      | true, Some e -> Some [Parsing_issues.Decodes_attribute_node (e, id, node)])
  | _ -> None
let detect_decoded_in_attr _ id _ _ node survived encoding = function
  | Ast.Payload_context.In_attribute -> (
      match (survived, encoding) with
      | _,Some Encodings.Unencoded -> None
      | false, _ -> None
      | true, None -> failwith "Can't happen"
      | true, Some e -> Some [Parsing_issues.Decodes_attribute_node (e, id, node)])
  | _ -> None
let detect_decoded _ id _ _ node survived encoding = function
  | Ast.Payload_context.In_text -> (
      match (survived, encoding) with
      | _, Some Encodings.Unencoded -> None
      | false, _ -> None
      | true, None -> failwith "Can't happen"
      | true, Some e -> Some [Parsing_issues.Decodes_text_node (e, id, node)])
  | _ -> None

let is_text = function
  | Ast.Text(_)  -> true
  | _ -> false

let has_tag_content = function
  | Ast.Tag(tn, _, N.HTML, cs) as t ->
    if List.exists (function | Ast.Tag(_,_,_,_) -> true | Ast.Comment _ -> true | Ast.Cdata _ -> true | _ -> false) cs then
      Some(t)
    else None
  | _ -> None

let detect_problematic_text_tag tag_names verbose id sanitizer_name parsed _ _ _ _ =
  let tags = Ast.to_tag_list parsed in
  tag_names |> List.filter_map (fun tag_name ->
    let text_tags = List.filter (function | Ast.Tag(tn,_,N.HTML,_) when String.equal_caseless tn tag_name -> true | _ -> false) tags in
  let problems = text_tags |> List.filter_map has_tag_content in 
      match problems with 
      | [] -> None
      | t :: tl -> Some [Parsing_issues.Text_tag_with_non_text_content(tag_name, id, t)]
    ) |> List.flatten |> function | [] -> None | x -> Some(x)

let detect_missing_namespace_foreign_breakup verbose id sanitizer_name parsed _ _ _ _ =
  let twps = Ast.tag_list_with_parents parsed in
  let is_foreign = function
    | Ast.Tag(_,_,N.SVG, _)
    | Ast.Tag(_,_,N.MathML, _) -> true
    | _ -> false 
  in
  let r = List.find_map (function | _,None -> None | t,Some p ->
      if is_foreign p && (Ast.foreign_content_breaker t) && (not @@ Ast.is_html_integration_point p) then
        let () = if verbose then
          Fmt.pr "%s (%d) does not break on %a in %a\n%!" sanitizer_name id Ast.pp t Ast.pp p
        else () in
        Some [Parsing_issues.Failure_to_break_foreign (id, t, p)]
      else None
    ) twps
  in r
let detect_bang_comment_issue _ id sanitizer_name _ node _ _ = function
  | Ast.Payload_context.In_comment ->
      if comment_containing_bang sanitizer_name node then
        Some [Parsing_issues.Comment_parsing_not_detecting_bang (id, node)]
      else None
  | _ -> None

(** TODO: this makes zero sense *)
let detect_content_after_plaintext verbose id sanitizer_name parsed node survived encoding  context = 
  let open Ast in
  let tags = to_tag_list parsed in
  let rec detect lst = 
    match lst with
    | [] -> None
    | Tag("plaintext", _, N.HTML, cs) as t :: xs -> 
      if List.for_all is_text cs then
        detect xs
      else
        Some[ Parsing_issues.Allows_to_close_plaintext(id, t)]
    |  _ :: tl -> detect tl
  in
  detect tags

let detect_issues ctx verbose id sanitizer_name context parsed node survived encoding exec =
  let detectors =
    [
      detect_wrong_cdata;
      detect_bang_comment_issue;
      detect_survived_in_text;
      detect_decoded;
      detect_decoded_in_attr;
      detect_decoded_in_comment;
      detect_content_after_plaintext;
      detect_missing_namespace_foreign_breakup;
      detect_problematic_text_tag ["noscript";"noembed";"noframes";"style";"textarea";"xmp";"iframe";"title";"noscript";"plaintext"]
    ]
  in
  let issues =
    detectors |> List.map (fun detector -> detector verbose id sanitizer_name parsed node survived encoding context)
  in
  let issues = issues  |> List.filter_map Fun.id |> List.flatten in
  if exec && survived && List.is_empty issues then
    Fmt.epr "%d with %s executed but found no issue. Payload was in %a..!\n%!" id sanitizer_name Ast.Payload_context.pp context else
  issues |> List.iter (fun issue -> add_parsing_issue ctx sanitizer_name issue)

let print_issues ctx verbose =
  let module Pi = Parsing_issues in
  let errors = ctx.errors |> Hashtbl.to_list in
  errors
  |> List.iter (fun (name, err) ->
         let errs = err |> Bounded_list.content in
         match errs with
         | [] -> ()
         | _ ->
             Fmt.epr "'%s' has the following errors:\n%!" name;
             errs |> List.iter (fun pe -> Fmt.epr "\t%a\n%!" Processing_error.fmt pe));
  let issues = ctx.issues |> Hashtbl.to_list in
  issues |> List.iter (fun (name, issues) ->
      let issue_names = Hashtbl.keys_list issues in
      let issues' = issue_names |> List.map (fun k -> let tbl = Hashtbl.get_or ~default:(Bounded_list.create 0) issues k in (k, (Bounded_list.content tbl))) in
      let compacted = issues' |> List.map (fun (k, vals) ->
          let ids = vals |> List.map Pi.get_id in
          Fmt.str "%s: (%a)" k (Fmt.list ~sep:(Fmt.any ", ") Fmt.int) ids
        ) in
           Fmt.pr "%s has the following parsing issues:\n\t[%a]\n%!" name
             (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
             compacted)

let analyse_sanitizer_result state verbose lookups reference_table parsed_ht (gen_id, sanitizer_id, browser_id, mode_id, payload, pretty, serialized, output, dom, result, exec) =
  let module Pc = Ast.Payload_context in
  (*Fmt.pr "Analysing result of gen id: %d\n%!" gen_id;*)
  let existing = ParsedHt.find_opt parsed_ht (gen_id, sanitizer_id) in
  match existing with
  | Some _ -> ()
  | None -> (begin
  let sanitizer_name = Hashtbl.get_or ~default:"ERROR" lookups.L.sanitizers sanitizer_id in
  let browser_name = Hashtbl.get_or ~default:"ERROR" lookups.L.browsers browser_id in
  let mode_name = Hashtbl.get_or ~default:"ERROR" lookups.L.modes mode_id in
  if verbose then
    let (_,_,ref_dom,_,_) = ReferenceHt.get reference_table (gen_id, browser_id, mode_id) |> Option.get_exn_or "No entry in reference table" in
    let browser_ast = Html_parser.process' ref_dom in
    let browser_ast = browser_ast |> Result.fold
                ~error:(fun e -> Fmt.epr "Error parsing html: %s\n%!" serialized)
                ~ok:(fun p ->
                  let postprocessor = Hashtbl.get_or Preprocessing.browser_postprocess browser_name ~default:Fun.id in
                  let parsed' = postprocessor p in
                  let context, _ = Ast.detect_payload_context ~id:gen_id parsed' in
                  if verbose then Fmt.pr "In %a: %a\n%!" Pc.pp context Ast.pp parsed' else
                    ()) in ()
  else ();
                  (*Fmt.pr "In %a\n%!" Pc.pp context) in*)
  (*Fmt.pr "%s -> %s (%s)\n%!" sanitizer_name output serialized;*)
           let parsed = Html_parser.process' serialized in
           parsed
           |> Result.fold
                ~error:(fun _ -> add_issue state sanitizer_name (Processing_error.Parsing_error (serialized, gen_id)))
                ~ok:(fun p ->
                  let postprocessor =
                    Hashtbl.get_or Preprocessing.sanitizer_postprocess sanitizer_name ~default:Fun.id
                  in
                  let parsed' = postprocessor p in
                  let parsed' = Ast.assign_namespaces parsed' in
                  let ctx' = Ast.detect_payload_context ~id:gen_id parsed' in
                  (*let parents = get_path_to_payload parsed' in*)
                  let survived = payload_survived output in
                  if survived && verbose then
                    Fmt.pr "%s (%d): Payload survived in output (%s)\n%!"  sanitizer_name gen_id output else ();
                  let encoding = if survived then Some(Encodings.determine_encoding pretty) else None in
                  ParsedHt.add parsed_ht (gen_id, sanitizer_id) true;
                  (match ctx' with
                  | context, Some node -> detect_issues state verbose gen_id sanitizer_name context parsed' node survived encoding exec
                  | _ -> ());
                  let ctx, _ = ctx' in
                  if verbose then ( 
                    if survived then Fmt.pr "In %a and survived %b: %a\n%!" Pc.pp ctx survived Ast.pp parsed' else ();
                    (*Fmt.pr "Path to payload: %a\n%!" (Fmt.list ~sep:(Fmt.any " -> ") Ast.pp_short) parents)*)
                  )
                  else ());
                    (*Fmt.pr "In %a and survived: %b\n%!" Pc.pp ctx survived);*)
  ()
  end)
  (*let _, payload, pretty, _, _, _, _ = sanitizer_results |> List.hd in*)
  (*Fmt.pr "%s (%s):\n\n%!" payload pretty;*)
  (*browser_results*)
  (*|> List.iter (fun (_, _, _, browser, mode, output, serialized, executed) ->*)
  (*       Fmt.pr "%a with %a exec: %b -> %s (%s)\n%!" Util.Browser.pp browser Util.Mode.pp mode executed output*)
  (*         serialized;*)
  (*       let parsed = Html_parser.process' serialized in*)
  (*       parsed*)
  (*let filter (_, _, _, sanitizer, _, _, _) = not (String.equal sanitizer.Util.Sanitizer.name "no-sanitizer") in*)
  (*let sanitizer_results = sanitizer_results |> List.filter filter in*)
  (*if List.length sanitizer_results > 0 then*)
  (*  let _, _, json, _, _, _, _ = List.hd sanitizer_results in*)
  (*  sanitizer_results*)
  (*  |> List.iter (fun (_, _, json, sanitizer, output, serialized, _) ->*)
  (*else ()*)
