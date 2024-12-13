open Containers
module TT = Tokenizer__Common.Token_tag

module Namespace = struct
  type t = HTML | SVG | MathML | None [@@deriving show {with_path = false}, eq, hash]

let pp_short fmt = function
  | HTML -> Fmt.pf fmt "%s" "h:"
  | SVG -> Fmt.pf fmt "%s" "s:"
  | MathML -> Fmt.pf fmt "%s" "m:"
  | None -> Fmt.pf fmt "%s" ""

end

type attr = { key : string; value : string } [@@deriving show { with_path = false }, eq, hash]

type t =
  | Tag of (string * attr list * Namespace.t * t list)
  | Text of string
  | Comment of string
  | Cdata of string
  | Document_fragment of t list
  | Document_type of string
  | Document of t list
[@@deriving show { with_path = false }, eq, hash]

let pp_short fmt = function
  | Document _ -> Fmt.pf fmt "%s" "#document"
  | Document_fragment _ -> Fmt.pf fmt "%s" "#document-fragment"
  | Document_type t -> Fmt.pf fmt "#document-type %s" t
  | Tag (name, attrs, ns, _) -> Fmt.pf fmt "#tag %a%s [%a]" Namespace.pp_short ns name (Fmt.list ~sep:(Fmt.any ", ") pp_attr) attrs
  | Text text -> Fmt.pf fmt "#text %s" text
  | Comment text -> Fmt.pf fmt "#comment %s" text
  | Cdata text -> Fmt.pf fmt "#cdata %s" text

let tag_list_with_parents ast =
  let rec flatten' p = function
    | Cdata _ as c -> [ (c, p) ]
    | Comment _ as c -> [ (c, p) ]
    | Text _ as c -> [ (c, p) ]
    | Tag (name, _, _, cs) as t ->
        let cs' = cs |> List.flat_map (flatten' (Some t)) in
        (t, p) :: cs'
    | Document_fragment cs | Document cs -> cs |> List.flat_map (flatten' None)
    | Document_type _ -> []
  in
  flatten' None ast
(** Pre order collection into a list *)
let to_tag_list ast =
  let rec flatten' = function
    | Cdata _ as c -> [ c ]
    | Comment _ as c -> [ c ]
    | Text _ as c -> [ c ]
    | Tag (name, _, _, cs) as t ->
        let cs' = cs |> List.flat_map flatten' in
        t :: cs'
    | Document_fragment cs | Document cs -> cs |> List.flat_map flatten'
    | Document_type _ -> []
  in
  flatten' ast

module N = Namespace
let is_html_integration_point = function
  | Tag("mi", _, N.MathML, _)
  | Tag("mo", _, N.MathML, _)
  | Tag("mn", _, N.MathML, _)
  | Tag("ms", _, N.MathML, _) -> true
  | Tag("annotation-xml", attrs, N.MathML, _) -> 
      if (List.exists (fun {key;value} -> String.equal_caseless key "encoding" && (String.equal_caseless value "application/xhtml+xml" || String.equal_caseless value "text/html")) attrs) then
          true
      else false
  | Tag("foreignObject", _, N.SVG, _)
  | Tag("foreignobject", _, N.SVG, _)
  | Tag("desc", _, N.SVG, _)
  | Tag("title", _, N.SVG, _) -> true
  | _ -> false

let foreign_content_breaker = function
  | Tag(n, _, _, _) when (List.mem ~eq:String.equal_caseless n ["b"; "big"; "blockquote"; "body"; "br"; "center"; "code"; "dd"; "div"; "dl"; "dt"; "em"; "embed"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "head"; "hr"; "i"; "img"; "li"; "listing"; "menu"; "meta"; "nobr"; "ol"; "p"; "pre"; "ruby"; "s"; "small"; "span"; "strong"; "strike"; "sub"; "sup"; "table"; "tt"; "u"; "ul"; "var"]) -> true
| _ -> false

let assign_namespaces ast =
  let rec assign ns node =
    match (ns, node) with
    | _, Document cs -> let cs' = List.map (assign ns) cs in Document cs'
    | _, Document_fragment cs -> let cs' = List.map (assign ns) cs in Document_fragment cs'
    | _, Document_type dt -> Document_type dt
    | _, (Text _ as n) -> n
    | _, (Comment _ as n) -> n
    | _, (Cdata _ as n) -> n
    | N.HTML, Tag("svg", attrs, _, cs) ->
      let cs' = cs |> List.map (assign N.SVG) in
      Tag("svg", attrs, N.SVG, cs')
    | N.HTML, Tag("math", attrs, _, cs) ->
      let cs' = cs |> List.map (assign N.MathML) in
      Tag("math", attrs, N.MathML, cs')
    (** MathML text integration points *)
    | N.MathML, Tag("mi" as n, attrs, _, cs)
    | N.MathML, Tag("mo" as n, attrs, _, cs)
    | N.MathML, Tag("mn" as n, attrs, _, cs)
    | N.MathML, Tag("ms" as n, attrs, _, cs)
    | N.MathML, Tag("mtext" as n, attrs, _, cs) ->
      let cs' = cs |> List.map (assign N.HTML) in
      Tag(n, attrs, N.MathML, cs')
    | N.MathML, Tag("annotation-xml" as n, attrs, _, cs) ->
      let nsb = if (List.exists (fun {key;value} -> String.equal_caseless key "encoding" && (String.equal_caseless value "application/xhtml+xml" || String.equal_caseless value "text/html")) attrs) then
          N.HTML
      else
        N.MathML
      in
        let cs' = cs |> List.map (assign nsb) in
        Tag(n, attrs, N.MathML, cs')
    | N.SVG, Tag("foreignObject" as n, attrs, _, cs)
    | N.SVG, Tag("foreignobject" as n, attrs, _, cs)
    | N.SVG, Tag("desc" as n, attrs, _, cs)
    | N.SVG, Tag("title" as n, attrs, _, cs) ->
      let cs' = cs |> List.map (assign N.HTML) in
      Tag(n, attrs, N.SVG, cs')
    (** Parse errors: https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inforeign *)
    | N.SVG, Tag("b" as n, attrs, _, cs) 
    | N.SVG, Tag("big" as n, attrs, _, cs)
    | N.SVG, Tag("blockquote" as n, attrs, _, cs)
    | N.SVG, Tag("body" as n, attrs, _, cs)
    | N.SVG, Tag("br" as n, attrs, _, cs)
    | N.SVG, Tag("center" as n, attrs, _, cs)
    | N.SVG, Tag("code" as n, attrs, _, cs)
    | N.SVG, Tag("dd" as n, attrs, _, cs)
    | N.SVG, Tag("div" as n, attrs, _, cs)
    | N.SVG, Tag("dl" as n, attrs, _, cs)
    | N.SVG, Tag("dt" as n, attrs, _, cs)
    | N.SVG, Tag("em" as n, attrs, _, cs)
    | N.SVG, Tag("embed" as n, attrs, _, cs)
    | N.SVG, Tag("head" as n, attrs, _, cs)
    | N.SVG, Tag("hr" as n, attrs, _, cs)
    | N.SVG, Tag("i" as n, attrs, _, cs)
    | N.SVG, Tag("img" as n, attrs, _, cs)
    | N.SVG, Tag("li" as n, attrs, _, cs)
    | N.SVG, Tag("listing" as n, attrs, _, cs)
    | N.SVG, Tag("menu" as n, attrs, _, cs)
    | N.SVG, Tag("meta" as n, attrs, _, cs)
    | N.SVG, Tag("nobr" as n, attrs, _, cs)
    | N.SVG, Tag("ol" as n, attrs, _, cs)
    | N.SVG, Tag("p" as n, attrs, _, cs)
    | N.SVG, Tag("ruby" as n, attrs, _, cs)
    | N.SVG, Tag("s" as n, attrs, _, cs)
    | N.SVG, Tag("small" as n, attrs, _, cs)
    | N.SVG, Tag("span" as n, attrs, _, cs)
    | N.SVG, Tag("strong" as n, attrs, _, cs)
    | N.SVG, Tag("strike" as n, attrs, _, cs)
    | N.SVG, Tag("sub" as n, attrs, _, cs)
    | N.SVG, Tag("sup" as n, attrs, _, cs)
    | N.SVG, Tag("table" as n, attrs, _, cs)
    | N.SVG, Tag("tt" as n, attrs, _, cs)
    | N.SVG, Tag("u" as n, attrs, _, cs)
    | N.SVG, Tag("ul" as n, attrs, _, cs)
    | N.SVG, Tag("var" as n, attrs, _, cs)
    | N.MathML, Tag("b" as n, attrs, _, cs) 
    | N.MathML, Tag("big" as n, attrs, _, cs)
    | N.MathML, Tag("blockquote" as n, attrs, _, cs)
    | N.MathML, Tag("body" as n, attrs, _, cs)
    | N.MathML, Tag("br" as n, attrs, _, cs)
    | N.MathML, Tag("center" as n, attrs, _, cs)
    | N.MathML, Tag("code" as n, attrs, _, cs)
    | N.MathML, Tag("dd" as n, attrs, _, cs)
    | N.MathML, Tag("div" as n, attrs, _, cs)
    | N.MathML, Tag("dl" as n, attrs, _, cs)
    | N.MathML, Tag("dt" as n, attrs, _, cs)
    | N.MathML, Tag("em" as n, attrs, _, cs)
    | N.MathML, Tag("embed" as n, attrs, _, cs)
    | N.MathML, Tag("head" as n, attrs, _, cs)
    | N.MathML, Tag("hr" as n, attrs, _, cs)
    | N.MathML, Tag("i" as n, attrs, _, cs)
    | N.MathML, Tag("img" as n, attrs, _, cs)
    | N.MathML, Tag("li" as n, attrs, _, cs)
    | N.MathML, Tag("listing" as n, attrs, _, cs)
    | N.MathML, Tag("menu" as n, attrs, _, cs)
    | N.MathML, Tag("meta" as n, attrs, _, cs)
    | N.MathML, Tag("nobr" as n, attrs, _, cs)
    | N.MathML, Tag("ol" as n, attrs, _, cs)
    | N.MathML, Tag("p" as n, attrs, _, cs)
    | N.MathML, Tag("ruby" as n, attrs, _, cs)
    | N.MathML, Tag("s" as n, attrs, _, cs)
    | N.MathML, Tag("small" as n, attrs, _, cs)
    | N.MathML, Tag("span" as n, attrs, _, cs)
    | N.MathML, Tag("strong" as n, attrs, _, cs)
    | N.MathML, Tag("strike" as n, attrs, _, cs)
    | N.MathML, Tag("sub" as n, attrs, _, cs)
    | N.MathML, Tag("sup" as n, attrs, _, cs)
    | N.MathML, Tag("table" as n, attrs, _, cs)
    | N.MathML, Tag("tt" as n, attrs, _, cs)
    | N.MathML, Tag("u" as n, attrs, _, cs)
    | N.MathML, Tag("ul" as n, attrs, _, cs)
    | N.MathML, Tag("var" as n, attrs, _, cs) ->
      let cs' = cs |> List.map (assign N.HTML) in
      Tag(n, attrs, N.HTML, cs')
    | _,Tag(n,attrs, _, cs) ->
      let cs' = cs |> List.map (assign ns) in
      Tag(n, attrs, ns, cs')
  in assign N.HTML ast

let contains_payload text =
  (* TODO: <script><img src=x onerror=mxss()></script> would match the second clause, is this an issue? *)
  let func_idx = String.find ~sub:"mxss" text in
  if func_idx < 0 then
    false 
  else
    let text = String.replace ~sub:"noscript" ~by:"noscrpt" text in
    (* HACK: to not break it by <noscript><script>mxss()</script> *)
    let img_idx = String.find ~sub:"img" text in
    let image_idx = String.find ~sub:"image" text in
    let src_idx = String.find ~sub:"src" text in
    let onerror_idx = String.find ~sub:"onerror" text in
    let fst_script_idx = String.find ~sub:"script" text in
    let snd_script_idx = String.find ~start:(fst_script_idx + 10) ~sub:"script" text in
    (img_idx >= 0 || image_idx >= 0)
    && (src_idx > img_idx || src_idx > image_idx)
    && onerror_idx > src_idx && func_idx > onerror_idx
    || (fst_script_idx >= 0 && func_idx > fst_script_idx && snd_script_idx > func_idx)

let attr_contains_payload attr = contains_payload attr.value || contains_payload attr.key

let src = { key = "src"; value = "x" }
let handler = { key = "onerror"; value = "mxss(1)" }

let matching_payloads attrs =
  attrs |> List.exists (fun attr -> String.equal attr.key src.key && String.equal attr.value src.value)
  && attrs |> List.exists (fun attr -> String.equal attr.key handler.key && String.equal attr.value handler.value)

let is_payload_tag = function
  | Tag ("script", _, _, [ Text "mxss(1)" ]) -> true
  | Tag ("image", attrs, _, _)
  | Tag ("img", attrs, _, _) -> matching_payloads attrs
  | _ -> false

let is_payload = function
  | Tag (_, attrs, _, _) as tag ->
      let in_attrs = List.exists attr_contains_payload attrs in
      in_attrs || is_payload_tag tag
  | Comment text | Cdata text | Text text -> contains_payload text
  | Document_fragment _ | Document _ | Document_type _ -> false

module Payload_context = struct
  type t = Regular | In_text | In_comment | In_attribute | In_cdata | Missing
  [@@deriving show { with_path = false }, eq]
end

let get_ctx ?(verbose = false) ?id ast contexts =
  let module Pc = Payload_context in
  let positives = contexts |> List.find_all (function Pc.Missing, None -> false | _ -> true) in
  match positives with
  | [] -> (Pc.Missing, None)
  | [x] -> x
  | x::_ ->
      if (not verbose) || Int.equal (List.count (function Pc.In_attribute, _ -> true | _ -> false) positives) (List.length positives)
      then x
      else
        let ids = match id with None -> "" | Some i -> Fmt.str "For %d: " i in
        Fmt.pr "%sFound %d contexts for '%a': %a\n%!" ids (List.length positives) pp ast
          (Fmt.list (Fmt.pair ~sep:(Fmt.any ", ") Pc.pp (Fmt.option pp)))
          positives;
        x 

let rec detect_payload_context ?(verbose = false) ?id ast =
  let module Pc = Payload_context in
  match ast with
  | Document_fragment cs | Document cs ->
      cs |> List.map (detect_payload_context ~verbose ?id) |> get_ctx ~verbose ?id ast
  | Tag (_, attrs, _, cs) as t ->
      if is_payload_tag t then (Pc.Regular, Some ast)
      else
        let in_attrs = List.exists attr_contains_payload attrs in
        if in_attrs then (Pc.In_attribute, Some ast)
        else cs |> List.map (detect_payload_context ?id) |> get_ctx ~verbose ?id ast
  | Comment text -> if contains_payload text then (Pc.In_comment, Some ast) else (Pc.Missing, None)
  | Text text -> if contains_payload text then (Pc.In_text, Some ast) else (Pc.Missing, None)
  | Cdata text -> if contains_payload text then (Pc.In_cdata, Some ast) else (Pc.Missing, None)
  | Document_type _ -> (Pc.Missing, None)

let cmp_depth = function
  | Some (ld, _), Some (rd, _) -> Int.compare ld rd
  | Some (d, _), None -> Int.compare d (-1)
  | None, Some (d, _) -> Int.compare (-1) d
  | None, None -> 0

let payload_depth ast =
  let rec payload_depth' depth acc ast : (int * t list) option =
    match ast with
    | Tag (_, _, _, children) as tag -> (
        if is_payload tag then Some (depth, [ tag ] @ acc)
        else
          let depths = children |> List.map (payload_depth' (depth + 1) ([ tag ] @ acc)) |> List.rev in
          let max =
            depths |> List.sort (fun l r -> cmp_depth (l, r)) |> List.rev |> List.head_opt |> function
            | Some s -> s
            | None -> None
          in
          match max with None -> None | Some (d, _) as r -> if d > 0 then r else None)
    | (Document_fragment children | Document children) as d -> (
        let depths = children |> List.map (payload_depth' (depth + 1) ([ d ] @ acc)) |> List.rev in
        let max =
          depths |> List.sort (fun l r -> cmp_depth (l, r)) |> List.rev |> List.head_opt |> function
          | Some s -> s
          | None -> None
        in
        match max with None -> None | Some (d, _) as r -> if d > 0 then r else None)
    | (Comment _ | Text _ | Cdata _) as t -> if is_payload t then Some (depth, [ t ] @ acc) else None
    | Document_type _ -> None
  in
  payload_depth' 0 [] ast

let payload_depths asts =
  let depths = asts |> List.map payload_depth in
  let max =
    depths |> List.sort (fun l r -> cmp_depth (l, r)) |> List.rev |> List.head_opt |> function
    | Some s -> s
    | None -> None
  in
  match max with None -> None | Some (d, _) as r -> if d > 0 then r else None

let of_tokens (tokens : Tokenizer__Common.general_token list) =
  let attr_converter attrs = attrs |> List.map (fun (k, v) -> { key = k; value = v }) in
  let is_self_closing name = Tag.self_closing_tags |> List.count (fun n -> String.equal n name) > 0 in
  let rec in_tag tag_name acc = function
    | [] -> (acc, [])
    | t :: ts -> (
        match t with
        | `Start TT.{ name; attributes; self_closing } ->
            let attrs = attr_converter attributes in
            if self_closing || is_self_closing name then
              let tag = Tag (name, attrs, None, []) in
              in_tag tag_name (acc @ [ tag ]) ts
            else
              let children, ts = in_tag name [] ts in
              let tag = Tag (name, attrs, None, children) in
              in_tag tag_name (acc @ [ tag ]) ts
        | `End TT.{ name; attributes; self_closing } ->
            let _ = (attributes, self_closing) in
            if String.equal name tag_name then (acc, ts) else in_tag tag_name acc ts
        | `String str -> in_tag tag_name (acc @ [ Text str ]) ts
        | `Comment str -> in_tag tag_name (acc @ [ Comment str ]) ts
        | `EOF -> (acc, [])
        | _ -> in_tag tag_name acc ts)
  and main' (acc : t list) (tokens : Tokenizer__Common.general_token list) =
    match tokens with
    | [] -> acc
    | t :: ts -> (
        match t with
        | `String str -> main' (acc @ [ Text str ]) ts
        | `Comment str -> main' (acc @ [ Comment str ]) ts
        | `EOF -> acc
        | `Start TT.{ name; attributes; self_closing } ->
            let attrs = attr_converter attributes in
            if self_closing || is_self_closing name then
              let tag = Tag (name, attrs, None, []) in
              main' (acc @ [ tag ]) ts
            else
              let children, ts = in_tag name [] ts in
              let tag = Tag (name, attrs, None, children) in
              main' (acc @ [ tag ]) ts
        | _ -> main' acc ts)
  in
  main' [] tokens

let of_string input =
  let tokens = Tokenizer.tokenize input |> List.map (fun (_, t) -> t) in
  of_tokens tokens
