open Mxssy

let uri_encoded_payload = "%3Cimg%20src%3Dx%20onerror%3Dmxss(1)%3E"
let at = Alcotest.testable Ast.pp Ast.equal

let test_matching_payload result tag () =
  let open Ast in
  Alcotest.(check bool) "matching_payload1" result (is_payload tag)

let get_parent_names lst =
  let open Ast in
  lst
  |> List.map (function
       | Tag (n, _, _, _) -> n
       | Comment _ -> "#comment"
       | Cdata _ -> "#cdata"
       | Text _ -> "#text"
       | Document_fragment _ -> "#document-fragment"
       | Document_type _ -> "#document-type"
       | Document _ -> "#document")

let test_payload_nesting result ast () =
  let open Ast in
  let r = payload_depth ast in
  let a = match r with Some (_, a) -> a | None -> [] in
  let a = a |> get_parent_names in
  Alcotest.(check (list string)) "payload_nesting" result a

let test_flatten result ast () =
  let a = Ast.to_tag_list ast in
  let a = a |> get_parent_names in
  Alcotest.(check (list string)) "flatten_test" result a

let test_payload_depth result ast () =
  let open Ast in
  let r = payload_depth ast in
  let d = match r with Some (d, _) -> d | None -> -1 in
  Alcotest.(check int) "payload_depth" result d

let nest_in_div depth tag =
  let open Ast in
  let module N = Namespace in
  let rec nest_in_div' depth acc =
    match depth with 0 -> acc | n -> nest_in_div' (n - 1) (Tag ("div", [], N.None, [ acc ]))
  in
  nest_in_div' depth tag

let test_namespaces inp result () =
  let inp' = Ast.assign_namespaces inp in
  Alcotest.(check at) "ast assignment" result inp'

let () =
  let open Ast in
  let module N = Namespace in
  Alcotest.run "Tag Checks"
    [
      ( "namespaces",
        [
          Alcotest.test_case "ns1" `Quick (test_namespaces (Tag ("img", [], N.None, [])) (Tag ("img", [], N.HTML, [])));
          Alcotest.test_case "ns2" `Quick
            (test_namespaces
               (Tag
                  ( "div",
                    [],
                    N.None,
                    [
                      Tag
                        ( "svg",
                          [],
                          N.None,
                          [ Tag ("math", [], N.None, [ Tag ("desc", [], N.None, [ Tag ("foo", [], N.None, []) ]) ]) ] );
                      Tag ("svg", [], N.None, []);
                    ] ))
               (Tag
                  ( "div",
                    [],
                    N.HTML,
                    [
                      Tag
                        ( "svg",
                          [],
                          N.SVG,
                          [ Tag ("math", [], N.SVG, [ Tag ("desc", [], N.SVG, [ Tag ("foo", [], N.HTML, []) ]) ]) ] );
                      Tag ("svg", [], N.SVG, []);
                    ] )));
          Alcotest.test_case "ns3" `Quick
            (test_namespaces
               (Tag
                  ( "math",
                    [],
                    N.None,
                    [
                      Tag
                        ( "mtext",
                          [],
                          N.None,
                          [ Tag ("svg", [], N.None, [ Tag ("desc", [], N.None, [ Tag ("foo", [], N.None, []) ]) ]) ] );
                      Tag ("annotation-xml", [], N.None, [ Tag ("foo", [], N.None, []) ]);
                    ] ))
               (Tag
                  ( "math",
                    [],
                    N.MathML,
                    [
                      Tag
                        ( "mtext",
                          [],
                          N.MathML,
                          [ Tag ("svg", [], N.SVG, [ Tag ("desc", [], N.SVG, [ Tag ("foo", [], N.HTML, []) ]) ]) ] );
                      Tag ("annotation-xml", [], N.MathML, [ Tag ("foo", [], N.MathML, []) ]);
                    ] )));
          Alcotest.test_case "ns4" `Quick
            (test_namespaces
               (Tag
                  ( "math",
                    [],
                    N.None,
                    [
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "text/html" } ],
                          N.None,
                          [ Tag ("foo", [], N.None, []) ] );
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "application/xhtml+xml" } ],
                          N.None,
                          [ Tag ("foo", [], N.None, []) ] );
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "bla" } ],
                          N.None,
                          [ Tag ("foo", [], N.None, []) ] );
                    ] ))
               (Tag
                  ( "math",
                    [],
                    N.MathML,
                    [
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "text/html" } ],
                          N.MathML,
                          [ Tag ("foo", [], N.HTML, []) ] );
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "application/xhtml+xml" } ],
                          N.MathML,
                          [ Tag ("foo", [], N.HTML, []) ] );
                      Tag
                        ( "annotation-xml",
                          [ { key = "encoding"; value = "bla" } ],
                          N.MathML,
                          [ Tag ("foo", [], N.MathML, []) ] );
                    ] )));
          Alcotest.test_case "ns4" `Quick
            (test_namespaces
               (Tag
                  ( "math",
                    [],
                    N.None,
                    [
                      Tag ("img", [], N.None, [ Tag ("foo", [], N.None, []) ]);
                      Tag ("foo", [], N.None, [ Tag ("div", [], N.None, []) ]);
                    ] ))
               (Tag
                  ( "math",
                    [],
                    N.MathML,
                    [
                      Tag ("img", [], N.HTML, [ Tag ("foo", [], N.HTML, []) ]);
                      Tag ("foo", [], N.MathML, [ Tag ("div", [], N.HTML, []) ]);
                    ] )));
          Alcotest.test_case "ns5" `Quick
            (test_namespaces
               (Tag
                  ( "svg",
                    [],
                    N.None,
                    [
                      Tag ("img", [], N.None, [ Tag ("foo", [], N.None, []) ]);
                      Tag ("foo", [], N.None, [ Tag ("div", [], N.None, []) ]);
                    ] ))
               (Tag
                  ( "svg",
                    [],
                    N.SVG,
                    [
                      Tag ("img", [], N.HTML, [ Tag ("foo", [], N.HTML, []) ]);
                      Tag ("foo", [], N.SVG, [ Tag ("div", [], N.HTML, []) ]);
                    ] )));
        ] );
      ( "flatten_testing",
        [
          Alcotest.test_case "flatten1" `Quick (test_flatten [ "img" ] (Tag ("img", [], N.None, [])));
          Alcotest.test_case "flatten2" `Quick
            (test_flatten [ "div"; "img"; "pre"; "div" ]
               (Tag
                  ( "div",
                    [],
                    N.None,
                    [ Tag ("img", [], N.None, [ Tag ("pre", [], N.None, []) ]); Tag ("div", [], N.None, []) ] )));
          Alcotest.test_case "flatten3" `Quick
            (test_flatten
               [ "div"; "pre"; "b"; "a"; "script"; "img"; "pre"; "div"; "math"; "noframes"; "iframe" ]
               (Tag
                  ( "div",
                    [],
                    N.None,
                    [
                      Tag
                        ( "pre",
                          [],
                          N.None,
                          [ Tag ("b", [], N.None, [ Tag ("a", [], N.None, [ Tag ("script", [], N.None, []) ]) ]) ] );
                      Tag ("img", [], N.None, [ Tag ("pre", [], N.None, []) ]);
                      Tag
                        ( "div",
                          [],
                          N.None,
                          [
                            Tag
                              ("math", [], N.None, [ Tag ("noframes", [], N.None, []); Tag ("iframe", [], N.None, []) ]);
                          ] );
                    ] )));
        ] );
      ( "payload_nesting",
        [
          Alcotest.test_case "payload_depth1" `Quick
            (test_payload_nesting [ "img" ]
               (Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, [])));
        ] );
      ( "payload_depth",
        [
          Alcotest.test_case "payload_depth1" `Quick
            (test_payload_depth 0
               (Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, [])));
          Alcotest.test_case "payload_depth2" `Quick
            (test_payload_depth 1
               (Tag
                  ( "div",
                    [],
                    N.None,
                    [
                      Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []);
                    ] )));
          Alcotest.test_case "payload_depth3" `Quick (test_payload_depth (-1) (Tag ("div", [], N.None, [])));
          Alcotest.test_case "payload_depth4" `Quick
            (test_payload_depth 1
               (nest_in_div 1
                  (Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []))));
          Alcotest.test_case "payload_depth4" `Quick
            (test_payload_depth 2
               (nest_in_div 2
                  (Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []))));
          Alcotest.test_case "payload_depth5" `Quick
            (test_payload_depth 1 (Tag ("div", [], N.None, [ Text uri_encoded_payload ])));
          Alcotest.test_case "payload_depth6" `Quick
            (test_payload_depth 1 (Tag ("div", [], N.None, [ Comment uri_encoded_payload ])));
          Alcotest.test_case "payload_depth7" `Quick
            (test_payload_depth 4
               (Tag
                  ( "div",
                    [ { key = "id"; value = "elem" } ],
                    N.None,
                    [
                      Tag
                        ( "math",
                          [ { key = "id"; value = "</desc>/*<p id=" }; { key = "<mglyph"; value = "" } ],
                          N.None,
                          [] );
                      Tag
                        ( "dl",
                          [],
                          N.None,
                          [
                            Tag
                              ( "desc",
                                [],
                                N.None,
                                [
                                  Tag
                                    ( "noembed",
                                      [],
                                      N.None,
                                      [ Text "<ms id=<pre><img src=x onerror=mxss(1)></pre>></title>-->--!>" ] );
                                ] );
                          ] );
                      Text "\">*/";
                      Comment "[CDATA[\"";
                    ] )));
          Alcotest.test_case "payload_depth8" `Quick
            (test_payload_depth 6
               (Tag
                  ( "div",
                    [ { key = "id"; value = "elem" } ],
                    N.None,
                    [
                      Tag
                        ( "math",
                          [ { key = "id"; value = "</desc>/*<p id=" }; { key = "<mglyph"; value = "" } ],
                          N.None,
                          [
                            Tag
                              ( "dl",
                                [],
                                N.None,
                                [
                                  Tag
                                    ( "desc",
                                      [],
                                      N.None,
                                      [
                                        Tag
                                          ( "noembed",
                                            [],
                                            N.None,
                                            [
                                              Tag
                                                ( "ms",
                                                  [ { key = "id"; value = "<pre" } ],
                                                  N.None,
                                                  [
                                                    Tag
                                                      ( "img",
                                                        [
                                                          { key = "src"; value = "x" };
                                                          { key = "onerror"; value = "mxss(1)" };
                                                        ],
                                                        N.None,
                                                        [] );
                                                    Text ">-->--!>";
                                                  ] );
                                            ] );
                                      ] );
                                ] );
                            Text "\">*/\">";
                          ] );
                    ] )));
        ] );
      ( "matching_payload",
        [
          Alcotest.test_case "matching_payload1" `Quick (test_matching_payload false (Tag ("img", [], N.None, [])));
          Alcotest.test_case "matching_payload2" `Quick
            (test_matching_payload false (Tag ("img", [ { key = "src"; value = "x" } ], N.None, [])));
          Alcotest.test_case "matching_payload3" `Quick
            (test_matching_payload true
               (Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, [])));
          Alcotest.test_case "matching_payload4" `Quick
            (test_matching_payload true
               (Tag
                  ( "img",
                    [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ],
                    N.None,
                    [ Tag ("div", [], N.None, []) ] )));
          Alcotest.test_case "matching_payload5" `Quick
            (test_matching_payload false
               (Tag
                  ( "img",
                    [ { key = "src"; value = "y" }; { key = "onerror"; value = "mxss(1)" } ],
                    N.None,
                    [ Tag ("div", [], N.None, []) ] )));
          Alcotest.test_case "matching_payload6" `Quick
            (test_matching_payload false
               (Tag
                  ( "div",
                    [ { key = "src"; value = "y" }; { key = "onerror"; value = "mxss(1)" } ],
                    N.None,
                    [ Tag ("div", [], N.None, []) ] )));
          Alcotest.test_case "payload_text1" `Quick (test_matching_payload true (Text uri_encoded_payload));
          Alcotest.test_case "payload_text2" `Quick (test_matching_payload false (Text "imgfoo"));
          Alcotest.test_case "payload_text3" `Quick (test_matching_payload false (Text "imgmxs"));
          Alcotest.test_case "payload_text4" `Quick
            (test_matching_payload true (Text "&lt;img src=x onerror=mxss(1)&gt;"));
          Alcotest.test_case "payload_text5" `Quick
            (test_matching_payload true (Text "<ms id=<pre><img src=x onerror=mxss(1)></pre>></title>-->--!>"));
          Alcotest.test_case "payload_comment1" `Quick (test_matching_payload true (Comment uri_encoded_payload));
          Alcotest.test_case "payload_comment2" `Quick (test_matching_payload false (Comment "imgfoo"));
          Alcotest.test_case "payload_comment3" `Quick (test_matching_payload false (Comment "imgmxs"));
          Alcotest.test_case "payload_comment4" `Quick
            (test_matching_payload true (Comment "&lt;img src=x onerror=mxss(1)&gt;"));
          Alcotest.test_case "payload_attr1" `Quick
            (test_matching_payload true
               (Tag
                  ( "div",
                    [ { key = "id"; value = "y<img src=x onerror=mxss(1)>Z" } ],
                    N.None,
                    [ Tag ("div", [], N.None, []) ] )));
          Alcotest.test_case "payload_attr2" `Quick
            (test_matching_payload false
               (Tag
                  ( "div",
                    [ { key = "id"; value = "y<img src=x" }; { key = "data-foo"; value = "onerror=mxss(1)>Z" } ],
                    N.None,
                    [ Tag ("div", [], N.None, []) ] )));
          (* TODO: Is this the behavior we want? *)
        ] );
    ]
