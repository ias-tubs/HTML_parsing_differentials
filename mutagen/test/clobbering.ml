open Mxssy
module C = Clobbering_analysis
module N = Ast.Namespace

let pp_pair pf (n, c) = Fmt.pf pf "%s: %d" n c
let eq_pair (ln, lc) (rn, rc) = String.equal ln rn && Int.equal lc rc
let pt = Alcotest.testable pp_pair eq_pair

let ast =
  Ast.(
    Tag
      ("div", [], N.None, [ Tag ("img", [], N.None, []); Tag ("div", [], N.None, [ Text "blablabla" ]); Text "foobar" ]))

let ast_id_img =
  Ast.(
    Tag
      ( "div",
        [],
        N.None,
        [
          Tag ("img", [ { key = "id"; value = "foo" } ], N.None, []);
          Tag ("div", [], N.None, [ Text "blablabla" ]);
          Text "foobar";
        ] ))

let ast_id_div =
  Ast.(
    Tag
      ( "div",
        [ { key = "id"; value = "foo" } ],
        N.None,
        [ Tag ("img", [], N.None, []); Tag ("div", [], N.None, [ Text "blablabla" ]); Text "foobar" ] ))

let ast'' =
  Ast.(
    Tag
      ( "div",
        [],
        N.None,
        [
          Tag ("img", [ { key = "id"; value = "foo" }; { key = "name"; value = "foo" } ], N.None, []);
          Tag ("div", [], N.None, [ Text "blablabla" ]);
          Text "foobar";
        ] ))

let matches l r () =
  let lids, lnames = C.get_ids_and_names_sorted l in
  let rids, rnames = C.get_ids_and_names_sorted r in
  Alcotest.(check (list pt)) "ids" lids rids;
  Alcotest.(check (list pt)) "names" lnames rnames

let () =
  Alcotest.run "Clobbering"
    [
      ( "clobbered",
        [
          Alcotest.test_case "Equal asts" `Quick (matches ast ast);
          Alcotest.test_case "Unequal asts" `Quick (matches ast_id_img ast_id_div);
          Alcotest.test_case "Unequal asts" `Quick (matches ast_id_div ast_id_img);
        ] );
    ]
