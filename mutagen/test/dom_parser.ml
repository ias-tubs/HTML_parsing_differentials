open Mxssy
open Containers
open Test_data
module N = Ast.Namespace

let at = Alcotest.testable Ast.pp Ast.equal

let test_dom_parser input cmp =
  let ast = Html_parser.process' input |> Result.get_exn in
  Fmt.pr "%a\n%!" Ast.pp ast;
  Alcotest.(check at) "parse" ast cmp

let test_dom_parser_3 () =
  let input =
    "(#tag document [] [(#comment \"[CDATA[<mtext\"), (#comment \"<input></table>\"), (#tag html [] [(#tag head [] \
     []), (#tag body [] [(#tag img [(#attr \"src\" \"x\"), (#attr \"onerror\" \"mxss(1)\")] []), (#text \"/*--!>\"), \
     (#tag input [] []), (#text \"--!>]]>\\\"\")])])])"
  in
  let cmp =
    Ast.(
      Tag
        ( "document",
          [],
          N.None,
          [
            Comment "[CDATA[<mtext";
            Comment "<input></table>";
            Tag
              ( "html",
                [],
                N.None,
                [
                  Tag ("head", [], N.None, []);
                  Tag
                    ( "body",
                      [],
                      N.None,
                      [
                        Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []);
                        Text "/*--!>";
                        Tag ("input", [], N.None, []);
                        Text "--!>]]>\"";
                      ] );
                ] );
          ] ))
  in
  test_dom_parser input cmp

let test_dom_parser_1 () =
  let input =
    "(#tag fragment [] [(#tag p [] [(#tag a [(#attr \"href\" \"http://example.com/\"), (#attr \"onclick\" \
     \"stealCookies()\")] [(#text \"Link\")])])])"
  in
  let cmp =
    Ast.(
      Tag
        ( "fragment",
          [],
          N.None,
          [
            Tag
              ( "p",
                [],
                N.None,
                [
                  Tag
                    ( "a",
                      [ { key = "href"; value = "http://example.com/" }; { key = "onclick"; value = "stealCookies()" } ],
                      N.None,
                      [ Text "Link" ] );
                ] );
          ] ))
  in
  test_dom_parser input cmp

let test_dom_parser_2 () =
  let input =
    "(#document [(#tag html [] [(#tag head [] []), (#tag body [] [(#tag select [] [(#tag template [] \
     [(#document-fragment [(#tag style [] [(#text \"<!--\")]), (#tag a [(#attr \"rel\" \
     \"--></style></template></select><img id=a src onerror=alert(1)>\")] [])])])])])])])"
  in
  let cmp =
    Ast.(
      Document
        [
          Tag
            ( "html",
              [],
              N.None,
              [
                Tag ("head", [], N.None, []);
                Tag
                  ( "body",
                    [],
                    N.None,
                    [
                      Tag
                        ( "select",
                          [],
                          N.None,
                          [
                            Tag
                              ( "template",
                                [],
                                N.None,
                                [
                                  Document_fragment
                                    [
                                      Tag ("style", [], N.None, [ Text "<!--" ]);
                                      Tag
                                        ( "a",
                                          [
                                            {
                                              key = "rel";
                                              value = "--></style></template></select><img id=a src onerror=alert(1)>";
                                            };
                                          ],
                                          N.None,
                                          [] );
                                    ];
                                ] );
                          ] );
                    ] );
              ] );
        ])
  in
  test_dom_parser input cmp

let parse_tests tests =
  tests
  |> List.map (fun input ->
         Alcotest.test_case "no changes" `Quick (fun () ->
             let ast = Html_parser.process' input |> Result.get_exn in
             Fmt.pr "%a\n%!" Ast.pp ast;
             Alcotest.(check string) "parse" "" ""))

let java_parse_tests = parse_tests Test_cases.java_tests
let ruby_parse_tests = parse_tests Test_cases.ruby_tests
let dompurify_tests = parse_tests Test_cases.dompurify_tests

let () =
  Alcotest.run "DOMParser"
    [
      ( "parse",
        [
          Alcotest.test_case "Parse simple DOM" `Quick test_dom_parser_1;
          Alcotest.test_case "Parse with fragments" `Quick test_dom_parser_2;
          Alcotest.test_case "Parse with fragments" `Quick test_dom_parser_3;
        ] );
      ("java", java_parse_tests);
      ("ruby", ruby_parse_tests);
      ("dompurify", dompurify_tests);
    ]
