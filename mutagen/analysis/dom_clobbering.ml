open Containers
open Mxssy
module Ts = Timings
module B = Util.Browser
module M = Util.Mode
module C = Clobbering_analysis

module ClobberingHt = CCHashtbl.Make (struct
  type t = B.t * M.t * int

  let equal (lb, lm, lid) (rb, rm, rid) = Int.equal lb.B.id rb.B.id && Int.equal lid rid && Int.equal lm.M.id rm.M.id

  let hash (b, m, id) =
    let bi = b.B.id in
    let mi = m.M.id in
    (Hash.triple Hash.int Hash.int Hash.int) (bi, mi, id)
end)

let clobbering_data = ClobberingHt.create 100

let print_timings initial_ts post_db_ts post_analysis_ts final_ts =
  let duration_total = Ts.diff initial_ts final_ts in
  let duration_db = Ts.diff initial_ts post_db_ts in
  let duration_analysis = Ts.diff post_db_ts post_analysis_ts in
  let duration_print = Ts.diff post_analysis_ts final_ts in
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr duration_total
    Ts.pr duration_db Ts.pr duration_analysis Ts.pr duration_print

let analyse_clobbering id data =
  let preprocess_ast b s =
    let ast = Html_parser.process' s |> Result.get_exn in
    let pp = Hashtbl.get_or Preprocessing.browser_postprocess b.B.name ~default:Fun.id in
    let ast' = pp ast in
    ast'
  in
  data
  |> List.iter (fun (browser, mode, payload, result, serialized) ->
         let ast = preprocess_ast browser serialized in
         let potentials = C.analyse_clobbering ast in
         if List.is_empty potentials then ()
         else
           let clob = C.Clobbering.{ id; browser; mode; payload; result; potentials } in
           ClobberingHt.add clobbering_data (browser, mode, id) clob)

let browser_data_tbl data =
  let tbl = Hashtbl.create (List.length data) in
  let () =
    data
    |> List.iter (fun (id, payload, browser, mode, result, serialized, _) ->
           Hashtbl.add_list tbl id (browser, mode, payload, result, serialized))
  in
  tbl

let print_stats () =
  let data = clobbering_data |> ClobberingHt.to_list in
  let len = List.length data in
  data
  |> List.iter (fun ((b, m, id), clob) ->
         Fmt.pr "%d (%s/%s) [%s -> %s] -> %a\n%!" id b.B.name m.M.name clob.C.Clobbering.payload
           clob.C.Clobbering.result
           (Fmt.list ~sep:(Fmt.any ", ") C.pp_gadget)
           clob.C.Clobbering.potentials);
  Fmt.pr "For a total of %d potential DOM clobbering gadget occurrences!\n%!" len

let analyse verbose rl rr config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* browser_data = Db.get_browser_results_range_light pool rl rr in
     let post_db_ts = Timings.timestamp () in
     let browser_groups = browser_data_tbl browser_data in
     let keys = Hashtbl.keys_list browser_groups in
     let data = keys |> List.map (fun k -> (k, Hashtbl.get_or browser_groups ~default:[] k)) in
     data |> List.iter (fun (k, d) -> analyse_clobbering k d);
     let post_analysis_ts = Timings.timestamp () in
     print_stats ();
     let final_ts = Timings.timestamp () in
     print_timings initial_ts post_db_ts post_analysis_ts final_ts;
     Lwt.return ());
  `Ok ()

let analyse_id verbose (id : int) config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* browser_data = Db.get_browser_results pool id in
     let post_db_ts = Timings.timestamp () in
     let browser_data = browser_data |> List.map (fun (_, p, _, b, m, o, s, _) -> (b, m, p, o, s)) in
     let () = analyse_clobbering id browser_data in
     let post_analysis_ts = Timings.timestamp () in
     let final_ts = Timings.timestamp () in
     print_timings initial_ts post_db_ts post_analysis_ts final_ts;
     Lwt.return ());
  `Ok ()

let analyse' verbose lb ub id config_fname =
  match id with None -> analyse verbose lb ub config_fname | Some i -> analyse_id verbose i config_fname

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let lb =
    let doc = "Lower bound of ID range. Default: 1" in
    Arg.(value & opt int 1 & info [ "l"; "lbound" ] ~doc)
  in
  let ub =
    let doc = "Upper bound of ID range. Default: 25000" in
    Arg.(value & opt int 25000 & info [ "u"; "ubound" ] ~doc)
  in
  let id =
    let doc = "Analyse $(docv) only" in
    Arg.(value & opt (some int) None & info [ "i"; "id" ] ~docv:"ID" ~doc)
  in
  let config_fname =
    let doc = "Configuration file" in
    Arg.(value & opt string "config.toml" & info [ "config" ] ~doc)
  in
  let doc = "Analyse browser behavior" in
  let info = Cmd.info "browser_analysis" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const analyse' $ verbose $ lb $ ub $ id $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
