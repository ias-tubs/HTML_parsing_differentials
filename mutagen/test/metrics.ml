open Mxssy
module N = Ast.Namespace

let test_bag_of_xpaths result ast () =
  let module M = Metrics.BagOfXPaths in
  let bag = M.from_ast ast |> M.XPaths.elements in
  let s = List.map (fun b -> Fmt.str "%a" M.fmt b) bag in
  Alcotest.(check (list string)) "xpaths" result s

let () =
  let open Ast in
  Alcotest.run "BagOfXPath"
    [
      ( "xpath similarity",
        [
          Alcotest.test_case "xpath1" `Quick
            (test_bag_of_xpaths [ "root/img[0]" ]
               (Document
                  [ Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []) ]));
          Alcotest.test_case "xpath2" `Quick
            (test_bag_of_xpaths
               [
                 "root/comment[1]";
                 "root/comment[0]";
                 "root/html[0]/head[0]";
                 "root/html[0]/body[0]/img[0]";
                 "root/html[0]/body[0]/text[1]";
                 "root/html[0]/body[0]/text[0]";
                 "root/html[0]/body[0]/input[0]";
               ]
               (Document_fragment
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
                                Tag
                                  ( "img",
                                    [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ],
                                    N.None,
                                    [] );
                                Text "/*--!>";
                                Tag ("input", [], N.None, []);
                                Text "--!>]]>\"";
                              ] );
                        ] );
                  ]));
        ] );
    ]
