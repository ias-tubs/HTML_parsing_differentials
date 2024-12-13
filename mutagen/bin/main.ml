open Containers
open Mxssy
module N = Ast.Namespace

let () =
  print_newline ();
  let module M = Metrics.BagOfXPaths in
  let _ =
    Ast.(Document [ Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []) ])
  in
  let ast =
    Ast.(
      Document_fragment
        [
          Tag
            ( "div",
              [],
              N.None,
              [
                Text "foo";
                Tag ("img", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], N.None, []);
                Text "bar";
              ] );
        ])
  in
  let _ =
    Ast.(
      Document_fragment
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
        ])
  in
  let bag = M.from_ast ast in
  bag |> M.XPaths.elements |> List.iter (fun b -> Fmt.pr "%a\n%!" M.pp_path b)
(*let open Gen in*)
(*let module O = Operation in*)
(*let module P = Placement in*)
(*let module A = Attribute in*)
(*let module Qd = Quoted in*)
(*let module Q = Quote in*)
(*let payload = Payload.Img_tag in*)
(*let fst = O.Close_tag (Tag.Noscript, P.Prepend) in*)
(*let snd = O.Enclose_tag_attribute (Tag.Div, (A.Id, Qd.Enclosed Q.Double)) in*)
(*let trd = O.Open_tag (Tag.Noscript, P.Prepend) in*)
(*let gen = [ O.Payload payload; fst; snd; trd ] in*)
(*let pretty = gen |> List.fold_left O.print "" in*)
(*Fmt.pr "%a: %s\n%!" Gen.pp gen pretty*)

(*let s = *)
(*  (*"(#document-fragment [(#comment \" foo\\\" \"), (#comment \"[CDATA[foo]]\"), (#tag noscript [  ] [(#comment \" xxx \"), (#tag div [ (#attr \"id\" \"</noscript><img src=x onerror=alert()>\") ] [])])])"*)*)
(*    "(#document-fragment [(#tag xmp [  ] [(#text \"</desc><dfn></svg></math><dl data-foo=' <noscript/><title>&lt;path&gt;&lt;pre&gt; --&gt;&lt;mo/&gt;&lt;iframe&gt;&lt;select&gt;&lt;img src=x onerror=mxss(1)&gt;&lt;/dl&gt;&lt;ms/&gt;&lt;/path&gt;</title>'/> <annotation-xml/> <form/></dfn>\")])])"*)
(*in*)
(*let r = Mxssy.Html_parser.process' s in*)
(*match r with*)
(*| Result.Ok(ast) -> Fmt.pr "%a\n%!" Mxssy.Ast.pp ast*)
(*| Result.Error(err) -> Fmt.pr "%s\n%!" err *)

(*let () = Mxssy.Html_parser.run ()*)
