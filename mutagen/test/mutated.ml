open Mxssy

let test_enclosing_tags_empty () =
  let open Gen in
  let module O = Operation in
  let g = [ O.Payload Payload.Img_tag ] in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "enclosing" [] enclosing_tags'

let test_enclosing_tags_minimal1 () =
  let open Gen in
  let module O = Operation in
  let g = [ O.Payload Payload.Img_tag; O.Enclose_tag (Tag.Div, []) ] in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "enclosing" [ "Div" ] enclosing_tags'

let test_enclosing_tags_minimal2 () =
  let open Gen in
  let module O = Operation in
  let g = [ O.Payload Payload.Img_tag; O.Open_tag (Tag.Div, [], Placement.Prepend) ] in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "enclosing" [ "Div" ] enclosing_tags'

let test_enclosing_tags_open_closed () =
  let open Gen in
  let module O = Operation in
  let g =
    [
      O.Payload Payload.Img_tag;
      O.Open_tag (Tag.Div, [], Placement.Prepend);
      O.Close_tag (Tag.Div, [], Placement.Prepend);
    ]
  in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "enclosing" [ "Div" ] enclosing_tags'

let test_enclosing_tags_closed_open () =
  let open Gen in
  let module O = Operation in
  let g =
    [
      O.Payload Payload.Img_tag;
      O.Close_tag (Tag.Div, [], Placement.Prepend);
      O.Open_tag (Tag.Div, [], Placement.Prepend);
    ]
  in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "enclosing" [] enclosing_tags'

let test_tags_in_xml_comments_1 () =
  let open Gen in
  let module O = Operation in
  let module P = Placement in
  let g =
    [
      O.Payload Payload.Img_tag;
      O.Close_XML_comment (Xml_comment_type.No_bang, P.Prepend);
      O.Enclose_tag (Tag.Div, []);
      O.Enclose_XML_comment Xml_comment_type.No_bang;
    ]
  in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "xml_comment" [] enclosing_tags'

let test_tags_in_xml_comments_2 () =
  let open Gen in
  let module O = Operation in
  let module P = Placement in
  let g =
    [
      O.Payload Payload.Img_tag;
      O.Close_XML_comment (Xml_comment_type.No_bang, P.Prepend);
      O.Enclose_tag (Tag.Div, []);
      O.Open_XML_comment P.Prepend;
    ]
  in
  let enclosing_tags = Mutated.gen_enclosing_tags g in
  let enclosing_tags' = enclosing_tags |> List.map Tag.show in
  Alcotest.(check (list string)) "xml_comment" [] enclosing_tags'

let () =
  Alcotest.run "Mutated"
    [
      ( "enclosed",
        [
          Alcotest.test_case "empty enclosing" `Quick test_enclosing_tags_empty;
          Alcotest.test_case "minimal enclosing: 1" `Quick test_enclosing_tags_minimal1;
          Alcotest.test_case "minimal enclosing: 2" `Quick test_enclosing_tags_minimal2;
          Alcotest.test_case "enclosing: open, closed" `Quick test_enclosing_tags_open_closed;
          Alcotest.test_case "enclosing: closed, open" `Quick test_enclosing_tags_closed_open;
        ] );
      ( "enclosed_with_xml_comments",
        [
          Alcotest.test_case "tags in xml comment: 1" `Quick test_tags_in_xml_comments_1;
          Alcotest.test_case "tags in xml comment: 2" `Quick test_tags_in_xml_comments_2;
        ] );
    ]
