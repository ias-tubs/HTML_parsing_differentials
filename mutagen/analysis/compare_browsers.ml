open Containers
open Mxssy
module Ts = Timings
module B = Util.Browser
module M = Util.Mode
module J = Metrics.Jaccard

module DiffHt = CCHashtbl.Make (struct
  type t = B.t * B.t * M.t

  let equal (lb1, lb2, lm) (rb1, rb2, rm) =
    Int.equal lb1.B.id rb1.B.id && Int.equal lb2.B.id rb2.B.id && Int.equal lm.M.id rm.M.id

  let hash (b1, b2, m) =
    let b1i = b1.B.id in
    let b2i = b2.B.id in
    let mi = m.M.id in
    (Hash.triple Hash.int Hash.int Hash.int) (b1i, b2i, mi)
end)

let print_timings initial_ts post_db_ts post_analysis_ts final_ts =
  let duration_total = Ts.diff initial_ts final_ts in
  let duration_db = Ts.diff initial_ts post_db_ts in
  let duration_analysis = Ts.diff post_db_ts post_analysis_ts in
  let duration_print = Ts.diff post_analysis_ts final_ts in
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr duration_total
    Ts.pr duration_db Ts.pr duration_analysis Ts.pr duration_print

let browser_delta = DiffHt.create 20

let analyse_difference mode lb ls rb rs =
  let tags_of_sample b s =
    let ast = Html_parser.process' s |> Result.get_exn in
    let pp = Hashtbl.get_or Preprocessing.browser_postprocess b.B.name ~default:Fun.id in
    let ast' = pp ast in
    J.from_ast ast'
  in
  let tagsl = tags_of_sample lb ls in
  let tagsr = tags_of_sample rb rs in
  let score = J.distance tagsl tagsr in
  Fmt.pr "Dissimlarity for %s<->%s and %s: %f\n%!" lb.B.name rb.B.name mode.M.name score;
  DiffHt.add_list browser_delta (lb, rb, mode) score

let analyse_sample id browser_data =
  Fmt.pr "Analysing sample %d!\n%!" id;
  let browser_data =
    browser_data |> List.fast_sort (fun (lb, _, _, _, _) (rb, _, _, _, _) -> Int.compare lb.B.id rb.B.id)
  in
  let modes =
    browser_data
    |> List.group_by
         ~hash:(fun (_, mode, _, _, _) -> Int.hash mode.M.id)
         ~eq:(fun (_, lm, _, _, _) (_, rm, _, _, _) -> Int.equal lm.M.id rm.M.id)
  in
  modes
  |> List.iter (fun samples ->
         let samples = samples |> List.diagonal in
         samples
         |> List.iter (fun ((lb, lm, _, _, ls), (rb, rm, _, _, rs)) ->
                (*Fmt.pr "%s.%s <-> %s.%s\n%!" lb.Util.Browser.name lm.Util.Mode.name rb.Util.Browser.name rm.Util.Mode.name;*)
                analyse_difference lm lb ls rb rs))

let browser_data_tbl data =
  let tbl = Hashtbl.create (List.length data) in
  let () =
    data
    |> List.iter (fun (id, payload, browser, mode, result, serialized, _) ->
           Hashtbl.add_list tbl id (browser, mode, payload, result, serialized))
  in
  tbl

let print_stats () =
  let keys = DiffHt.keys_list browser_delta in
  keys
  |> List.fast_sort (fun (_, _, lm) (_, _, rm) -> Int.compare lm.M.id rm.M.id)
  |> List.iter (fun ((lb, rb, mode) as k) ->
         let scores = DiffHt.get_or browser_delta ~default:[] k in
         let sorted = List.fast_sort Float.compare scores in
         let len = List.length sorted in
         let mid_idx = len / 2 in
         let median = List.get_at_idx_exn mid_idx sorted in
         let sum = sorted |> List.fold_left ( +. ) 0. in
         let avg = sum /. Float.of_int len in
         let max = List.get_at_idx_exn (len - 1) sorted in
         Fmt.pr "%s<->%s with %s has %f/%f/%f median/avg/max dissimlarity over %d samples\n%!" lb.B.name rb.B.name
           mode.M.name median avg max len)

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
     data |> List.iter (fun (k, d) -> analyse_sample k d);
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
     let () = analyse_sample id browser_data in
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
