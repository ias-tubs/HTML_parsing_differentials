open Mxssy
open Test_data

let pp_ipair pf (l, r) = Fmt.pf pf "(%d, %d)" l r
let eq_ipair (ll, lr) (rl, rr) = Int.equal ll rl && Int.equal lr rr
let ipt = Alcotest.testable pp_ipair eq_ipair
let pp_wp pf (l, r) = Fmt.pf pf "(%d, %a)" l (Fmt.option ~none:(Fmt.any "None") Fmt.int) r
let eq_wp (ll, lr) (rl, rr) = Int.equal ll rl && Option.equal Int.equal lr rr
let wpt = Alcotest.testable pp_wp eq_wp

let test_with_parent res inp () =
  let inp' = Util.with_parents inp in
  Alcotest.(check (list wpt)) "with parents" res inp'

let test_unchanged () =
  let lst = [ "b"; "c"; "d" ] in
  Alcotest.(check (list string)) "no change" lst (Util.drop_first String.equal "a" lst)

let test_removal () =
  let lst = [ "b"; "a"; "c"; "d" ] in
  Alcotest.(check (list string)) "removes" [ "b"; "c"; "d" ] (Util.drop_first String.equal "a" lst)

let test_removes_only_first () =
  let lst = [ "b"; "c"; "a"; "d"; "a" ] in
  Alcotest.(check (list string)) "only first" [ "b"; "c"; "d"; "a" ] (Util.drop_first String.equal "a" lst)

let test_take_until lst u res () =
  let r = Util.take_until_inc (fun n -> Int.equal n u) lst in
  Alcotest.(check (list int)) "take_until_inc" res r

let test_finds_max () =
  let lst = [ 1; 2; 3 ] in
  Alcotest.(check int) "finds max" 3 (Util.list_max Int.compare 0 lst)

let encode_uri_tests =
  Util_data.data
  |> List.map (fun (payload, enc, _) ->
         Alcotest.test_case "no changes" `Quick (fun () ->
             Alcotest.(check string) "same string" (Util.encode_uri payload) enc))

let encode_uri_component_tests =
  Util_data.data
  |> List.map (fun (payload, _, enc) ->
         Alcotest.test_case "no changes" `Quick (fun () ->
             Alcotest.(check string) "same string" (Util.encode_uri_component payload) enc))

let test_steps lst l u ss () = Alcotest.(check (list ipt)) "Simple_steps" lst (Util.make_steps l u ss)

let () =
  Alcotest.run "Utils"
    [
      ("with_parents", [ Alcotest.test_case "steps 1" `Quick (test_with_parent [ (1, None); (2, Some 1) ] [ 1; 2 ]) ]);
      ( "take_until_inc",
        [ Alcotest.test_case "take until inc 1" `Quick (test_take_until [ 1; 4; 7; 3; 9 ] 3 [ 1; 4; 7; 3 ]) ] );
      ( "make_steps",
        [
          Alcotest.test_case "steps 1" `Quick (test_steps [ (10, 20); (21, 30); (31, 40); (41, 50) ] 10 50 10);
          Alcotest.test_case "steps 2" `Quick
            (test_steps [ (200000, 300000); (300001, 400000); (400001, 500000) ] 200000 500000 100000);
        ] );
      ( "drop_first",
        [
          Alcotest.test_case "no changes" `Quick test_unchanged;
          Alcotest.test_case "removes needle" `Quick test_removal;
          Alcotest.test_case "removes needle only once" `Quick test_removes_only_first;
        ] );
      ("encode_uri", encode_uri_tests);
      ("encode_uri_component", encode_uri_component_tests);
      ("list_max", [ Alcotest.test_case "finds_max" `Quick test_finds_max ]);
    ]
