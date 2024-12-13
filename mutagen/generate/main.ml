open Containers
module IntSet = Set.Make (Int)

let init_hashes_from_db pool =
  let module Db = Mxssy.Database in
  let open Lwt.Infix in
  Db.get_hashes pool >>= fun init_hashes ->
  (*init_hashes |> List.map Int.to_string |> String.concat ", " |> print_endline;*)
  let hashes = ref (IntSet.of_list init_hashes) in
  (*!hashes |> IntSet.elements |> List.map Int.to_string |> String.concat ", " |> print_endline;*)
  (*"=====" |> print_endline;*)
  Lwt.return hashes

let generate_payloads hashes total state =
  let module Gen = Mxssy.Gen in
  let gens = ref [] in
  let total' = total + IntSet.cardinal !hashes in
  let tries = ref 0 in
  let i = ref 0 in
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
        (*(*json |> print_endline;*)*)
        let payload = generated |> List.fold_left Gen.Operation.print "" in
        (*payload |> print_endline;*)
        (*print_newline ();*)
        gens := (hash, payload, pretty, json) :: !gens
    | true -> ());
    hashes := IntSet.add hash !hashes;
    i := IntSet.cardinal !hashes
  done;
  (!gens, !tries)

let mxss verbose total (seed : int list) config_fname =
  let module Db = Mxssy.Database in
  let module Gen = Mxssy.Gen in
  let module Cfg = Mxssy.Config in
  let module Ts = Mxssy.Timings in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* pool = Db.connect config.Cfg.database in
     (*let* () = Db.test_proc pool "foobarcaml" in*)
     (*Fmt.pr "Got %s\n%!" s;*)
     let before_init = Ts.timestamp () in
     let* hashes = init_hashes_from_db pool in
     let after_init = Ts.timestamp () in
     let duration_init = Ts.diff before_init after_init in
     if IntSet.cardinal !hashes >= 12000000 then
       let _ = Fmt.pr "Currently at %d generations, terminating...\n%!" (IntSet.cardinal !hashes) in
       let* () = Lwt_unix.sleep 6000. in
       Lwt.return ()
     else
       let seed_text = seed |> List.map Int.to_string |> String.concat ", " |> Fmt.str "[%s]" in
       let seed = Array.of_list seed in
       Random.full_init seed;
       let* state_id = Db.add_rng_state pool seed_text in
       (*Fmt.pr "Added rng state (%s) with id %d\n%!" seed_text state_id;*)
       let state = Gen.init_state verbose in
       let before = Ts.timestamp () in
       let payloads, tries = generate_payloads hashes total state in
       let after = Ts.timestamp () in
       let duration = Ts.diff before after in
       let dur_per_payload = Ts.of_float_ns (Ts.to_float_ns duration /. (total |> Int.to_float)) in
       let before_db = Ts.timestamp () in
       let* () = payloads |> List.map (fun (h, p, pr, j) -> Db.add_generated pool h p pr j state_id) |> Lwt.join in
       let after_db = Ts.timestamp () in
       let duration_db = Ts.diff before_db after_db in
       Fmt.pr
         "Took %d tries and %a (%a pP) to generate %d payloads and DB access took %a for init and %a for inserting\n%!"
         tries Ts.pr duration Ts.pr dur_per_payload (List.length payloads) Ts.pr duration_init Ts.pr duration_db;
       Lwt.return ());
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
  let config_fname =
    let doc = "Configuration file" in
    Arg.(value & opt string "config.toml" & info [ "config" ] ~doc)
  in
  let doc = "generate mxss payloads" in
  let info = Cmd.info "gen" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const mxss $ verbose $ total $ seed $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
