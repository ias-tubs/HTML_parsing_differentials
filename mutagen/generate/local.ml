open Containers
module IntSet = Set.Make (Int)

let generate_payloads hashes total state =
  let module Gen = Mxssy.Gen in
  let gens = ref [] in
  let total' = total + IntSet.cardinal !hashes in
  let tries = ref 1 in
  let i = ref 1 in
  while !i < total' do
    tries := !tries + 1;
    let generated = Gen.generate state in
    let hash = Gen.hash_generated generated in
    (match IntSet.mem hash !hashes with
    | false ->
        (*hash |> Int.to_string |> print_endline;*)
        let pretty = generated |> List.map Gen.Operation.show |> String.concat ", " in
        (*pretty |> print_endline;*)
        let json = generated |> Gen.serialize in
        (*json |> print_endline;*)
        let payload = generated |> List.fold_left Gen.Operation.print "" in
        payload |> print_endline;
        (*Tokenizer.print_tokenz payload;*)
        print_newline ();
        gens := (hash, payload, pretty, json) :: !gens
    | true -> ());
    hashes := IntSet.add hash !hashes;
    i := IntSet.cardinal !hashes
  done;
  (!gens, !tries)

let mxss verbose total (seed : int list) =
  let module Ts = Mxssy.Timings in
  let module Gen = Mxssy.Gen in
  let seed_text = seed |> List.map Int.to_string |> String.concat ", " |> Fmt.str "[%s]" in
  print_endline seed_text;
  let seed = Array.of_list seed in
  Random.full_init seed;
  let state = Gen.init_state verbose in
  let before = Ts.timestamp () in
  let hashes = ref IntSet.empty in
  let _, tries = generate_payloads hashes total state in
  let after = Ts.timestamp () in
  let duration = Ts.diff before after in
  let dur_per_payload = Ts.of_float_ns (Ts.to_float_ns duration /. (total |> Int.to_float)) in
  Fmt.pr "Took %d tries and %a (%a pP) to generate %d payloads\n%!" tries Ts.pr duration Ts.pr dur_per_payload total;
  `Ok ()

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let total =
    let doc = "Generate $(docv) Payloads" in
    Arg.(value & opt int 10 & info [ "c"; "count" ] ~docv:"COUNT" ~doc)
  in
  let seed =
    let doc = "Set RNG seed to [|$(docv)|]" in
    Arg.(value & opt_all int [ 1; 1337; 666 ] & info [ "s"; "seed" ] ~docv:"Numbers" ~doc)
  in
  let doc = "generate mxss payloads" in
  let info = Cmd.info "gen" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const mxss $ verbose $ total $ seed))

let () = exit (Cmdliner.Cmd.eval cmdline)
