open Containers

let payload_in_xml_comment gen =
  let open Gen in
  let module O = Operation in
  let enc, man =
    gen
    |> List.fold_left
         (fun (e, o) c ->
           match c with
           | O.Open_XML_comment Placement.Prepend -> (e, o + 1)
           | O.Enclose_XML_comment _ -> (e + 1, o)
           | O.Close_XML_comment (_, Placement.Append) -> (e, o - 1)
           | _ -> (e, o))
         (0, 0)
  in
  enc + man > 0

let payload_was_uri_encoded gen =
  let open Gen in
  let module O = Operation in
  let f, c =
    gen
    |> List.fold_left
         (fun (regular, component) c ->
           match c with
           | O.EncodeURI -> (regular + 1, component)
           | O.EncodeURI_component -> (regular, component + 1)
           | _ -> (regular, component))
         (0, 0)
  in
  f + c > 0

let payload_was_xml_encoded gen =
  let open Gen in
  let module O = Operation in
  let enc = gen |> List.fold_left (fun count c -> match c with O.Xml_Encode -> count + 1 | _ -> count) 0 in
  enc > 0

let strip_commented_tags gen =
  let open Gen in
  let module O = Operation in
  let module P = Placement in
  let tags, _, commented =
    gen
    |> List.fold_left
         (fun (tags, in_xml_comment, commented) c ->
           match c with
           | O.Close_XML_comment (_, P.Prepend) -> (tags, true, commented)
           | O.Enclose_XML_comment _ -> (tags, false, [])
           | O.Open_XML_comment P.Prepend -> (tags, false, [])
           | t ->
               if in_xml_comment then (tags, in_xml_comment, t :: commented) else (t :: tags, in_xml_comment, commented))
         ([], false, [])
  in
  commented @ tags |> List.rev

let gen_enclosing_tags gen =
  let open Gen in
  let module O = Operation in
  let module P = Placement in
  (* TODO: Fix XML comment nesting
     Consider things like: [(Close_XML_comment Prepend); (Open_tag (Input, Prepend)); Enclose_XML_comment]
     Here the input tag should not be counted as enclosing, as it is in an XML comment *)
  let gen = gen |> strip_commented_tags in
  (*Printf.printf "%s\n%!" (Gen.show gen);*)
  let _, stack =
    gen
    |> List.fold_left
         (fun (closed, opened) c ->
           match c with
           | O.Enclose_tag (t, _) | O.Enclose_tag_attribute (t, _, _, _, Unclosed) -> (closed, t :: opened)
           | O.Open_tag (t, _, P.Prepend) ->
               (* If this tag has been closed before -> do not count this as additional nesting and remove closing tag from stack *)
               let closed' = Util.drop_first Equal.physical t closed in
               if Int.equal (List.length closed') (List.length closed) then (closed, t :: opened) else (closed', opened)
           | O.Close_tag (t, _, P.Prepend) -> (t :: closed, opened)
           | _ -> (closed, opened))
         ([], [])
  in
  (*stack |> List.map Tag.show |> String.concat ", " |> Printf.printf "%s\n%!";*)
  stack

let gen_payload_depth gen =
  let module T = Gen.Tag in
  let stack = gen_enclosing_tags gen in

  stack
  |> List.filter (function
       | T.Break | T.Input | T.Textarea | T.Keygen -> false
       (* Filter out self closing tags *)
       | _ -> true)
  |> List.length

let rec payload_depth' depth ast =
  let open Ast in
  match ast with
  | Tag (_, _, _, cs) ->
      if Score.is_payload ast then depth
      else
        let depths = cs |> List.map (payload_depth' (depth + 1)) in
        depths |> Util.list_max Int.compare 0
  | _ -> 0

let payload_depth ast =
  let open Ast in
  match ast with
  | Tag (_, _, _, cs) ->
      let depths = cs |> List.map (payload_depth' 0) in
      depths |> Util.list_max Int.compare 0
  | _ -> 0

let has_mutated gen_id _ eval_id json serialized result =
  Printf.printf "Working on %d '%s' -> '%s'\n%!" gen_id result serialized;
  let generated = Gen.deserialize json in
  let gen_depth = gen_payload_depth generated in
  let was_uri_encoded = payload_was_uri_encoded generated in
  let was_xml_encoded = payload_was_xml_encoded generated in
  let was_xml_commented = payload_in_xml_comment generated in
  let ast = Html_parser.process' serialized |> Result.get_exn in
  let ast_depth = payload_depth ast in
  Printf.printf "%d: %s (%d) -> %s (%d)\n(%b, %b, %b)\n%!" gen_id (Gen.show generated) gen_depth (Ast.show ast)
    ast_depth was_uri_encoded was_xml_encoded was_xml_commented;
  (* TODO: currently just checking for depth in the tree, we should also consider whether the nesting is based on different tags now *)
  let mutated = was_uri_encoded || was_xml_commented || was_xml_encoded || not (Int.equal gen_depth ast_depth) in
  (eval_id, mutated)
