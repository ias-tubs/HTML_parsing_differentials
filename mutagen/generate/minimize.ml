open Containers
open Mxssy

type gen_t = { payload : Gen.Operation.t; tokens : Gen.Operation.t list; length : int }

let gen_t_of_list lst payload =
  let length = List.length lst in
  { tokens = lst; length; payload }

let gen_t_to_gen g = g.payload :: g.tokens

let minimize _ gen =
  let rec minimize' gen cutoff : gen_t list =
    if cutoff > gen.length then [ gen ]
    else
      let indices = List.range' 0 gen.length in
      let reduced = indices |> List.map (fun i -> gen_t_of_list (List.remove_at_idx i gen.tokens) gen.payload) in
      reduced |> List.map (fun r -> minimize' r 3) |> List.flatten
  in
  minimize' gen 3

let gen_subseq arr =
  let rec gen_subseq' i j acc =
    if i <= 0 then acc
    else
      let acc' = if Int.equal 1 (Int.logand i 1) then acc @ [ Array.get arr i ] else acc in
      gen_subseq' (Int.shift_right i 1) (j + 1) acc'
  in
  let _ = List.range' 1 (Int.shift_left 1 (Array.length arr)) in
  let _ = gen_subseq' 5 0 [] in
  (*range |> List.iter( fun i -> Fmt.pr "%d\n%!" i )*)
  ()

let minimize gen =
  let minimized = List.sublists_of_len 6 ~offset:2 ~last:CCOption.return gen.tokens in
  Fmt.pr "%d\n%!" (List.length minimized);
  (*let tarr = gen.tokens |> Array.of_list in*)
  (*let _ = gen_subseq tarr in*)
  ()

let main verbose id out_fname config_fname =
  let _ = (verbose, out_fname) in
  let id = match id with None -> failwith "booh" | Some i -> i in
  let module Db = Database in
  let module Cfg = Config in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     (*let ( and* ) = Lwt.both in*)
     let config = Cfg.parse config_fname in
     let* pool = Db.connect config.Cfg.database in
     let* gen = Db.get_full_generation pool id in
     Fmt.pr "%a" Db.Generation.pp gen;
     let gen' = Gen.deserialize gen.Db.Generation.json in
     Fmt.pr "%a" Gen.pp gen';
     let payload = List.hd gen' in
     let tokens = List.drop 1 gen' in
     let g = gen_t_of_list tokens payload in
     let () = minimize g in
     Lwt.return ());
  `Ok ()

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let id =
    let doc = "Minimize generated payload with ID: $(docv)" in
    Arg.(value & opt (some int) None & info [ "i"; "id" ] ~docv:"ID" ~doc)
  in
  let config_fname =
    let doc = "Configuration file" in
    Arg.(value & opt string "config.toml" & info [ "config" ] ~doc)
  in
  let out_fname =
    let doc = "Output file" in
    Arg.(value & opt string "out.csv" & info [ "out" ] ~doc)
  in
  let doc = "Minimize a Payload" in
  let info = Cmd.info "minimize" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const main $ verbose $ id $ out_fname $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
