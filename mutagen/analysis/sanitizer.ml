open Containers
open Mxssy
module SA = Sanitizer_analysis
module Ts = Timings
module L = Util.Lookups

let print_timings total_span db_span analysis_span print_span =
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr total_span Ts.pr
    db_span Ts.pr analysis_span Ts.pr print_span

let get_lookups pool =
  let module Db = Database in
  let ( let* ) = Lwt.bind in
  let ( and* ) = Lwt.both in
  let* sanitizers = Db.get_eval_sanitizers_table pool
  and* browsers = Db.get_browsers_table pool
  and* modes = Db.get_modes_table pool in
  Lwt.return L.{ browsers; modes; sanitizers }

let analyse verbose lower_bound upper_bound chunks step_size csv out_dir config_fname =
  let module Db = Database in
  let module Cfg = Config in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let ( and* ) = Lwt.both in
     let steps =
       if upper_bound - lower_bound > step_size then Util.make_steps lower_bound upper_bound step_size
       else [ (lower_bound, upper_bound) ]
     in
     (*let ( and* ) = Lwt.both in*)
     Fmt.pr "%d -> %d: Steps length %d\n%!" lower_bound upper_bound (List.length steps);
     let chunks = List.chunks chunks steps in
     let config = Cfg.parse config_fname in
     let* pool = Db.connect config.Cfg.database in
     let* lookups = get_lookups pool in
     let sanitizers =
       Hashtbl.keys_list lookups.L.sanitizers
       |> List.map (fun id -> Hashtbl.get_or lookups.L.sanitizers ~default:"ERROR" id)
     in
     let ctx = SA.setup sanitizers in
     let initial_ts = Timings.timestamp () in
     let process_chunk l u =
       let () = Fmt.pr "Step %d->%d\n%!" l u in
       let pre_db_ts = Ts.timestamp () in
       let* data =
         Db.get_executed_range_light pool l u
         (*and* reference = Db.get_executed_reference_range_light pool l u in*)
       in
       let reference = [] in
       let post_db_ts = Ts.timestamp () in
       let parsed_ht = SA.ParsedHt.create step_size in
       let ref_tbl = SA.make_reference_table reference in
       data |> List.iter (SA.analyse_sanitizer_result ctx verbose lookups ref_tbl parsed_ht);
       let post_analysis_ts = Ts.timestamp () in
       Lwt.return (Ts.diff pre_db_ts post_db_ts, Ts.diff post_db_ts post_analysis_ts)
     in
     let* res = chunks |> Lwt_list.map_s (fun chunk -> chunk |> Lwt_list.map_p (fun (l, u) -> process_chunk l u)) in
     let res = List.flatten res in
     let db_span, analysis_span =
       res |> List.fold_left (fun (d, a) (ad, aa) -> (Ts.add d ad, Ts.add a aa)) (Ts.zero (), Ts.zero ())
     in
     let pre_print_ts = Timings.timestamp () in
     SA.print_issues ctx verbose;
     let final_ts = Timings.timestamp () in
     let print_span = Timings.diff final_ts pre_print_ts in
     let total_span = Timings.diff final_ts initial_ts in
     print_timings total_span db_span analysis_span print_span;
     Lwt.return ());
  `Ok ()

let analyse_id verbose (id : int) config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let ( and* ) = Lwt.both in
     let* pool = Db.connect config.Cfg.database in
     let* lookups = get_lookups pool in
     let sanitizers =
       Hashtbl.keys_list lookups.L.sanitizers
       |> List.map (fun id -> Hashtbl.get_or lookups.L.sanitizers ~default:"ERROR" id)
     in
     let ctx = SA.setup sanitizers in
     let* data = Db.get_executed_light pool id and* reference = Db.get_executed_reference_light pool id in
     let ref_tbl = SA.make_reference_table reference in
     let parsed_ht = SA.ParsedHt.create 100 in
     data |> List.iter (SA.analyse_sanitizer_result ctx verbose lookups ref_tbl parsed_ht);
     Lwt.return (SA.print_issues ctx verbose));
  `Ok ()

let read_data fname =
  let parse_line line =
    let sps = String.Split.left ~by:"," line in
    match sps with
    | Some (l, r) -> (
        (*Fmt.pr "%s <-> %s\n%!" l r;*)
        match (Int.of_string l, Int.of_string r) with Some l', Some r' -> Some (l', r') | _ -> None)
    | None -> None
  in
  let ls = IO.(with_in fname read_lines_l) in
  ls |> List.filter_map (fun l -> String.trim l |> parse_line)

let analyse' verbose id lb ub cs ss csv out_dir config_fname =
  match id with
  | None -> analyse verbose lb ub cs ss csv out_dir config_fname
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
  let doc = "generate mxss payloads" in
  let info = Cmd.info "gen" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const analyse' $ verbose $ id $ lb $ ub $ cs $ ss $ csv $ out_dir $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
