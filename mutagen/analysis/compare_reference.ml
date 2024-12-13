open Containers
open Mxssy
module Ts = Timings
module B = Util.Browser
module M = Util.Mode
module S = Util.Sanitizer
module J = Metrics.BagOfXPaths

let print_timings total_span db_span analysis_span print_span =
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr total_span Ts.pr
    db_span Ts.pr analysis_span Ts.pr print_span

type lookup_tables = {
  browsers : (int, string) Hashtbl.t;
  modes : (int, string) Hashtbl.t;
  sanitizers : (int, string) Hashtbl.t;
}
[@@deriving show { with_path = false }]

let get_lookups pool =
  let module Db = Database in
  let ( let* ) = Lwt.bind in
  let ( and* ) = Lwt.both in
  let* sanitizers = Db.get_sanitizers_table pool
  and* browsers = Db.get_browsers_table pool
  and* modes = Db.get_modes_table pool in
  Lwt.return { browsers; modes; sanitizers }

module DeltaHt = CCHashtbl.Make (struct
  type t = int * int * int

  let equal (lb, lm, ls) (rb, rm, rs) = Int.equal lb rb && Int.equal lm rm && Int.equal ls rs
  let hash (b, m, s) = (Hash.triple Hash.int Hash.int Hash.int) (b, m, s)
end)

let sanitizer_delta = DeltaHt.create 60

let analyse_sample ?(verbose = false) id browser_data sanitizer_data =
  browser_data
  |> List.iter (fun (browser, mode, payload, output, serialized, ref_ast) ->
         if verbose then
           Fmt.pr "Analysing sample %d, comparing against %a, %a: %s/%s\n%!" id B.pp browser M.pp mode payload
             serialized
         else ();
         let ref_tags = J.from_ast ref_ast in
         let sanitizer_data =
           sanitizer_data
           |> List.map (fun (sanitizer, serialized, ast) ->
                  let tags = J.from_ast ast in
                  (sanitizer, ast, tags))
         in
         sanitizer_data
         |> List.iter (fun (sanitizer, ast, tags) ->
                let score = J.similarity ref_tags tags in
                DeltaHt.add_list sanitizer_delta (browser.B.id, mode.M.id, sanitizer.S.id) score;
                if verbose then Fmt.pr "Simlarity for %a: %f\n%!" S.pp sanitizer score else ()))

let print_stats ?(csv = false) out_dir lookups =
  let ( let* ) = Lwt.bind in
  let sanitizers = DeltaHt.keys_list sanitizer_delta in
  let results = Hashtbl.create 20 in
  sanitizers
  |> List.iter (fun k ->
         let bid, mid, sid = k in
         let scores = DeltaHt.get_or sanitizer_delta ~default:[] k in
         let sorted = List.fast_sort Float.compare scores in
         let len = List.length sorted in
         let mid_idx = len / 2 in
         let median = List.get_at_idx_exn mid_idx sorted in
         let sum = sorted |> List.fold_left ( +. ) 0. in
         let avg = sum /. Float.of_int len in
         (*let max = List.get_at_idx_exn (len - 1) sorted in*)
         Hashtbl.add_list results sid (bid, mid, avg, median));
  let print_csv_stats out_dir results =
    let dt = Time.current_time () in
    let fname = Fmt.str "compare_ref-%a.csv" Time.pp dt in
    let fname' = Filename.concat out_dir fname in
    Lwt_io.with_file ~mode:Lwt_io.output fname' (fun ch ->
        let* () = Lwt_io.write_line ch "sanitizer;browser;header;average;median" in
        let* () =
          results
          |> Lwt_list.iter_s (fun (sanitizer, data) ->
                 data
                 |> Lwt_list.iter_s (fun (bid, mid, avg, median) ->
                        let browser = Hashtbl.get_or lookups.browsers ~default:"ERROR" bid in
                        let mode = Hashtbl.get_or lookups.modes ~default:"ERROR" mid in
                        Lwt_io.write_line ch @@ Fmt.str "%s;%s;%s;%f;%f" sanitizer browser mode avg median))
        in
        Fmt.pr "Wrote csv file to: %s\n%!" fname';
        Lwt.return ())
  in
  let sanitizer_results =
    results |> Hashtbl.keys_list
    |> List.map (fun sid ->
           let sn = Hashtbl.get_or lookups.sanitizers ~default:"ERROR" sid in
           let data = Hashtbl.get_or ~default:[] results sid in
           (sn, data))
    |> List.fast_sort (fun (ln, _) (rn, _) -> String.compare ln rn)
    |> List.map (fun (s, d) ->
           let d' =
             d
             |> List.fast_sort (fun (lb, lm, _, _) (rb, rm, _, _) ->
                    match Int.compare lb rb with 0 -> Int.compare lm rm | n -> n)
           in
           (s, d'))
  in
  sanitizer_results
  |> List.iter (fun (sanitizer, data) ->
         data
         |> List.iter (fun (bid, mid, avg, median) ->
                let browser = Hashtbl.get_or lookups.browsers ~default:"ERROR" bid in
                let mode = Hashtbl.get_or lookups.modes ~default:"ERROR" mid in
                Fmt.pr "%s <-> %s.%s has %f/%f avg/median simlarity\n%!" sanitizer browser mode avg median));
  if csv then print_csv_stats out_dir sanitizer_results else Lwt.return ()

let sanitizer_data_tbl data =
  let tbl = Hashtbl.create (List.length data) in
  data
  |> List.iter (fun (id, sanitizer, _, serialized) ->
         let ast = Html_parser.process' serialized in
         match ast with
         | Result.Error e ->
             Fmt.epr "Error parsing serialized DOM of %d for %a: %s\nDOM: %s\n%!" id S.pp sanitizer e serialized
         | Result.Ok ast ->
             let p = Hashtbl.get_or Preprocessing.sanitizer_postprocess sanitizer.S.name ~default:Fun.id in
             let ast = p ast in
             Hashtbl.add_list tbl id (sanitizer, serialized, ast));
  tbl

let browser_data_tbl data =
  let tbl = Hashtbl.create (List.length data) in
  data
  |> List.iter (fun (id, payload, browser, mode, result, serialized, _) ->
         let ref_ast = Html_parser.process' serialized |> Result.get_exn in
         let p = Hashtbl.get_or Preprocessing.browser_postprocess browser.B.name ~default:Fun.id in
         let ast = p ref_ast in
         Hashtbl.add_list tbl id (browser, mode, payload, result, serialized, ast));
  tbl

let analyse verbose tex csv out_dir lower_bound upper_bound chunks step_size config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let ( and* ) = Lwt.both in
     let* pool = Db.connect config.Cfg.database in
     let steps =
       if upper_bound - lower_bound > step_size then Util.make_steps lower_bound upper_bound step_size
       else [ (lower_bound, upper_bound) ]
     in
     (*let ( and* ) = Lwt.both in*)
     Fmt.pr "%d -> %d: Steps length %d\n%!" lower_bound upper_bound (List.length steps);
     let chunks = List.chunks chunks steps in
     let* lookups = get_lookups pool in
     let initial_ts = Timings.timestamp () in
     let process_chunk lower upper =
       let () = Fmt.pr "Step %d->%d\n%!" lower upper in
       let pre_db_ts = Timings.timestamp () in
       let* sanitizer_data = Db.get_sanitizer_results_range_light pool lower upper
       and* browser_data = Db.get_browser_results_range_light pool lower upper in
       let post_db_ts = Timings.timestamp () in
       let sanitizer_groups = sanitizer_data_tbl sanitizer_data in
       let browser_groups = browser_data_tbl browser_data in
       let keys = Hashtbl.keys_list browser_groups in
       let data =
         keys
         |> List.map (fun k ->
                (k, Hashtbl.get_or browser_groups ~default:[] k, Hashtbl.get_or sanitizer_groups ~default:[] k))
       in
       data |> List.iter (fun (key, browser_data', sanitizer_data') -> analyse_sample key browser_data' sanitizer_data');
       let post_analysis_ts = Timings.timestamp () in
       Lwt.return (Timings.diff pre_db_ts post_db_ts, Timings.diff post_db_ts post_analysis_ts)
     in
     let* res = chunks |> Lwt_list.map_s (fun chunk -> chunk |> Lwt_list.map_p (fun (l, u) -> process_chunk l u)) in
     let res = List.flatten res in
     let db_span, analysis_span =
       res |> List.fold_left (fun (d, a) (ad, aa) -> (Ts.add d ad, Ts.add a aa)) (Ts.zero (), Ts.zero ())
     in
     let pre_print_ts = Ts.timestamp () in

     let* () = print_stats ~csv out_dir lookups in
     let post_print_ts = Ts.timestamp () in
     let print_span = Ts.diff pre_print_ts post_print_ts in
     let total_span = Ts.diff initial_ts post_print_ts in
     print_timings total_span db_span analysis_span print_span;
     Lwt.return ());
  `Ok ()

let analyse_id verbose (id : int) config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Fmt.pr "Analysing sample with id %d\n%!" id;
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let ( and* ) = Lwt.both in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* sanitizer_data = Db.get_sanitizer_results_light pool id and* browser_data = Db.get_browser_results pool id in
     let reference_data =
       browser_data
       |> List.map (fun (id, payload, _, browser, mode, output, serialized, _) ->
              let ref_ast = Html_parser.process' serialized |> Result.get_exn in
              let p = Hashtbl.get_or Preprocessing.browser_postprocess browser.B.name ~default:Fun.id in
              let ast = p ref_ast in
              (browser, mode, payload, output, serialized, ast))
     in
     let post_db_ts = Timings.timestamp () in
     let sanitizer_data' =
       sanitizer_data
       |> List.map (fun (s, _, serialized) ->
              let ast = Html_parser.process' serialized in
              match ast with
              | Result.Error e ->
                  failwith
                  @@ Fmt.str "Error parsing serialized DOM of %d for %a: %s\nDOM: %s\n%!" id S.pp s e serialized
              | Result.Ok ast ->
                  let p = Hashtbl.get_or Preprocessing.sanitizer_postprocess s.S.name ~default:Fun.id in
                  let ast = p ast in
                  (s, serialized, ast))
     in
     let () = analyse_sample id reference_data sanitizer_data' in
     let post_analysis_ts = Timings.timestamp () in
     (*print_samples verbose (List.length data);*)
     let final_ts = Timings.timestamp () in
     print_timings initial_ts post_db_ts post_analysis_ts final_ts;
     Lwt.return ());
  `Ok ()

let analyse' verbose tex csv out_dir lb ub cs ss id config_fname =
  match id with
  | None -> analyse verbose tex csv out_dir lb ub cs ss config_fname
  | Some i -> analyse_id verbose i config_fname

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
  let tex =
    let doc = "Print tex files" in
    Arg.(value & flag & info [ "t"; "tex" ] ~doc)
  in
  let csv =
    let doc = "Print csv files" in
    Arg.(value & flag & info [ "csv" ] ~doc)
  in
  let cs =
    let doc = "Query n chunks of $(STEP-SIZE) in parallel" in
    Arg.(value & opt int 10 & info [ "c"; "chunks" ] ~docv:"chunks" ~doc)
  in
  let ss =
    let doc = "Step Size. Default: 100000" in
    Arg.(value & opt int 100000 & info [ "s"; "step-size" ] ~docv:"STEP-SIZE" ~doc)
  in
  let out_dir =
    let doc = "Output directory for tex files" in
    Arg.(value & opt dir "/tmp" & info [ "out" ] ~doc)
  in

  let doc = "Analyse browser behavior" in
  let info = Cmd.info "browser_analysis" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const analyse' $ verbose $ tex $ csv $ out_dir $ lb $ ub $ cs $ ss $ id $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
