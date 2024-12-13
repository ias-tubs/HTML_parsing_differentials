open Mxssy
module N = Ast.Namespace
module TT = Tokenizer__Common.Token_tag

let at = Alcotest.testable Ast.pp Ast.equal
let tag ?(self_closing = false) name attributes = { TT.name; attributes; self_closing }

let tags =
  [
    `Start (tag "div" []);
    `Start (tag ~self_closing:true "img" []);
    `Start (tag ~self_closing:false "div" []);
    `String "blablabla";
    `End (tag "div" []);
    `String "foobar";
  ]

let ast =
  Ast.
    [
      Tag
        ( "div",
          [],
          N.None,
          [ Tag ("img", [], N.None, []); Tag ("div", [], N.None, [ Text "blablabla" ]); Text "foobar" ] );
    ]

let ast' =
  Ast.
    [
      Tag
        ( "select",
          [],
          N.None,
          [
            Tag ("keygen", [], N.None, []);
            Tag
              ( "iframe",
                [ { key = "title"; value = "<table" } ],
                N.None,
                [
                  Tag
                    ( "mi",
                      [],
                      N.None,
                      [
                        Tag ("keygen", [], N.None, []);
                        Text "'";
                        Tag
                          ( "mglyph",
                            [],
                            N.None,
                            [
                              Tag
                                ( "img",
                                  [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ],
                                  N.None,
                                  [] );
                              Text "--!>";
                            ] );
                        Text "--!>";
                        Tag ("var", [], N.None, [ Tag ("pre", [], N.None, [ Text ">" ]) ]);
                      ] );
                ] );
          ] );
    ]

let ast'' =
  Ast.
    [
      Tag
        ( "select",
          [],
          N.None,
          [
            Tag ("keygen", [], N.None, []);
            Tag
              ( "iframe",
                [ { key = "title"; value = "<table" } ],
                N.None,
                [
                  Tag
                    ( "mi",
                      [],
                      N.None,
                      [
                        Tag ("keygen", [], N.None, []);
                        Text "'";
                        Tag
                          ( "style",
                            [],
                            N.None,
                            [
                              Tag
                                ( "img",
                                  [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ],
                                  N.None,
                                  [] );
                              Text "--!>";
                              Text "--!>";
                              Tag ("var", [], N.None, [ Tag ("pre", [], N.None, [ Text ">" ]) ]);
                            ] );
                      ] );
                ] );
          ] );
    ]

let matches' input ast () =
  let tokens = Tokenizer.tokenize input |> List.map (fun (_, t) -> t) in
  let ast' = Ast.of_tokens tokens in
  Alcotest.(check (list at)) "tokens to ast" ast ast'

let matches (tokens : Tokenizer__Common.general_token list) ast () =
  let ast' = Ast.of_tokens tokens in
  Alcotest.(check (list at)) "tokens to ast" ast ast'

let () =
  Alcotest.run "Tag Checks"
    [
      ( "tokens to ast",
        [
          Alcotest.test_case "ast of tokens" `Quick (matches tags ast);
          Alcotest.test_case "ast of string" `Quick
            (matches'
               "<select><keygen><iframe title=<table><mi><keygen>'<mglyph><img src=x \
                onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>"
               ast');
          Alcotest.test_case "ast of string" `Quick
            (matches'
               "<select><keygen><iframe title=<table><mi><keygen>'<style><img src=x \
                onerror=mxss(1)>--!></mglyph></keygen>--!></mtext><var><pre></mi></table></xmp>></keygen></select>"
               ast'');
        ] );
    ]
