open Containers
open Mxssy
module Ts = Timings
module B = Util.Browser
module M = Util.Mode
module S = Util.Sanitizer
module C = Clobbering_analysis
module J = Metrics.BagOfXPaths

type lookup_tables = {
  browsers : (int, string) Hashtbl.t;
  modes : (int, string) Hashtbl.t;
  sanitizers : (int, string) Hashtbl.t;
}
[@@deriving show { with_path = false }]

module ModeHt = CCHashtbl.Make (struct
  type t = int * int

  let equal (lb, lm) (rb, rm) = Int.equal lb rb && Int.equal lm rm
  let hash (b, m) = (Hash.pair Hash.int Hash.int) (b, m)
end)

let fill_mode_ht browsers data =
  let mode_ht = ModeHt.create 6 in
  data
  |> List.iter (fun (id, browser, mode, sanitizer, serialized, executed) ->
         let idht = ModeHt.get_or_add mode_ht ~f:(fun (_, _) -> Hashtbl.create 1000) ~k:(browser, mode) in
         let browser_name =
           Hashtbl.get browsers browser |> Option.get_exn_or (Fmt.str "Browser with id %d not found!" browser)
         in
         let pp = Hashtbl.get_or Preprocessing.browser_postprocess browser_name ~default:Fun.id in
         let ast = Html_parser.process' serialized |> Result.get_exn in
         let ast = pp ast in
         let metric = J.from_ast_benign ast in
         Hashtbl.add_list idht id (sanitizer, serialized, ast, metric, executed));
  mode_ht

let analyse_sample ?(verbose = false) lookups id browser mode data delta_tbl =
  let ref, sanitized =
    data
    |> List.partition (fun (sanitizer, _, _, _, _) ->
           String.equal (Hashtbl.get_or ~default:"ERROR" lookups.sanitizers sanitizer) "no-sanitizer")
  in
  let ref = List.head_opt ref in
  match ref with
  | None ->
      Fmt.pr "No unsanitized entry found for %d\n%!" id;
      ()
  | Some (_, _, _, ref_metric, _) ->
      let _ =
        sanitized
        |> List.iter (fun (sanitizer_id, _, _, metric, _) ->
               (*let metric = J.from_ast_benign ast in*)
               let similarity = J.similarity ~verbose ref_metric metric in
               if verbose then
                 let mode_name =
                   Hashtbl.get lookups.modes mode |> Option.get_exn_or (Fmt.str "Mode with id %d not found!" mode)
                 in
                 let sanitizer_name =
                   Hashtbl.get lookups.sanitizers sanitizer_id
                   |> Option.get_exn_or (Fmt.str "No sanitizer with id %d found" sanitizer_id)
                 in
                 let browser_name =
                   Hashtbl.get lookups.browsers browser
                   |> Option.get_exn_or (Fmt.str "Browser with id %d not found!" browser)
                 in
                 Fmt.pr "For %s/%s and %d: sanitizer %s similarity of %f\n%!" browser_name mode_name id sanitizer_name
                   similarity
               else ();
               Hashtbl.add_list delta_tbl sanitizer_id similarity;
               ())
      in
      ()

let delta_tbls lookups : (int, float list) Hashtbl.t ModeHt.t =
  let tbl = ModeHt.create 6 in
  let () =
    lookups.browsers |> Hashtbl.keys_list
    |> List.iter (fun b ->
           lookups.modes |> Hashtbl.keys_list
           |> List.iter (fun m ->
                  let delta_tbl = Hashtbl.create 20 in
                  ModeHt.add tbl (b, m) delta_tbl))
  in
  tbl

let analyse_mangled lookups verbose mode_ht delta_tbls =
  if verbose then
    let mode_ht_len = List.length (ModeHt.keys_list mode_ht) in
    Fmt.pr "Mode_ht has %d entries\n%!" mode_ht_len
  else ();
  ModeHt.keys_list delta_tbls
  |> List.iter (fun (ref_browser, ref_mode) ->
         let empty_tbl = Hashtbl.create 0 in
         let ref = ModeHt.get_or mode_ht (ref_browser, ref_mode) ~default:empty_tbl in
         if verbose then
           Fmt.pr "Mode_ht(%s,%s) has %d entries\n%!"
             (Hashtbl.get_or lookups.browsers ~default:"ERROR" ref_browser)
             (Hashtbl.get_or lookups.modes ~default:"ERROR" ref_mode)
             (List.length (Hashtbl.keys_list ref))
         else ();
         let delta_tbl = ModeHt.get_or ~default:(Hashtbl.create 0) delta_tbls (ref_browser, ref_mode) in
         ref |> Hashtbl.keys_list
         |> List.iter (fun id ->
                let data = Hashtbl.get_or ref ~default:[] id in
                analyse_sample ~verbose lookups id ref_browser ref_mode data delta_tbl;
                ());
         ())

let print_timings total_span db_span analysis_span print_span =
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr total_span Ts.pr
    db_span Ts.pr analysis_span Ts.pr print_span

let print_stats ?(tex = false) ?(csv = false) out_dir lookups (delta_tbls : (int, float list) Hashtbl.t ModeHt.t) =
  let ( let* ) = Lwt.bind in
  let bms =
    delta_tbls |> ModeHt.keys_list
    |> List.fast_sort (fun (lb, lm) (rb, rm) -> match Int.compare lb rb with 0 -> Int.compare lm rm | n -> n)
  in
  let print_formatted_stats res ~suffix ~(delim : string) ~line_break ~fmt =
    let data =
      res
      |> List.fast_sort (fun (ls, lb, lm, _, _) (rs, rb, rm, _, _) ->
             match String.compare ls rs with
             | 0 -> ( match B.cmp_name lb rb with 0 -> M.cmp_name lm rm | b -> b)
             | n -> n)
    in
    let dt = Time.current_time () in
    let fname = Fmt.str "mangled-%a.%s" Time.pp dt suffix in
    let fname' = Filename.concat out_dir fname in
    Lwt_io.with_file ~mode:Lwt_io.output fname' (fun ch ->
        let header = [ "sanitizer_name"; "browser_name"; "mode_name"; "average"; "median" ] in
        let header_line = Fmt.str "%s%s" (String.concat delim header) line_break in
        Fmt.pr "%s\n%!" header_line;
        let* () = Lwt_io.write_line ch header_line in
        let* () =
          data
          |> Lwt_list.iter_s (fun (sanitizer, browser, mode, average, median) ->
                 let data = [ sanitizer; browser; mode; fmt average; fmt median ] in
                 let line = Fmt.str "%s%s" (String.concat delim data) line_break in
                 Fmt.pr "%s\n%!" line;
                 Lwt_io.write_line ch line)
        in
        Fmt.pr "Wrote output to %s\n%!" fname';
        Lwt.return ())
  in
  let print_csv_stats data =
    print_formatted_stats data ~suffix:"csv" ~delim:" ; " ~line_break:"" ~fmt:(fun f -> Fmt.str "%g" f)
  in
  let res =
    bms
    |> List.map (fun (b, m) ->
           let browser_name = Hashtbl.get_or lookups.browsers ~default:"ERROR" b in
           let mode_name = Hashtbl.get_or lookups.modes ~default:"ERROR" m in
           Fmt.pr "For %s.%s:\n%!" browser_name mode_name;
           let delta_tbl = ModeHt.get_or delta_tbls ~default:(Hashtbl.create 0) (b, m) in
           let keys = Hashtbl.keys_list delta_tbl in
           let print_text_stats keys =
             keys
             |> List.map (fun k ->
                    let name =
                      Hashtbl.get lookups.sanitizers k |> Option.get_exn_or (Fmt.str "No sanitizer with id %d found" k)
                    in
                    (k, name))
             |> List.fast_sort (fun (_, l) (_, r) -> String.compare l r)
             |> List.map (fun (k, name) ->
                    (*let name =*)
                    (*  Hashtbl.get lookups.sanitizers k |> Option.get_exn_or (Fmt.str "No sanitizer with id %d found" k)*)
                    (*in*)
                    let data = Hashtbl.get_or delta_tbl k ~default:[] in
                    let len = List.length data in
                    let sorted = List.fast_sort Float.compare data in
                    let sum = data |> List.fold_left ( +. ) 0.0 in
                    let mid_idx = len / 2 in
                    let median = List.get_at_idx_exn mid_idx sorted in
                    let avg = sum /. Float.of_int len in
                    (*Hashtbl.add_list tbl name (avg, median);*)
                    Fmt.pr "\t%s (%s.%s) the average similarity is %f, the median %f\n%!" name browser_name mode_name
                      avg median;
                    (name, browser_name, mode_name, avg, median))
           in

           print_text_stats keys)
    |> List.flatten
  in
  if csv then print_csv_stats res else Lwt.return ()

let get_lookups pool =
  let module Db = Database in
  let ( let* ) = Lwt.bind in
  let ( and* ) = Lwt.both in
  let* sanitizers = Db.get_sanitizers_table pool
  and* browsers = Db.get_browsers_table pool
  and* modes = Db.get_modes_table pool in
  Lwt.return { browsers; modes; sanitizers }

let analyse verbose tex csv out_dir config_fname =
  let module Db = Database in
  let module Cfg = Config in
  let config = Cfg.parse config_fname in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     (*let ( and* ) = Lwt.both in*)
     let* pool = Db.connect config.Cfg.database in
     let* lookups = get_lookups pool in
     let delta_tbls = delta_tbls lookups in
     let initial_ts = Ts.timestamp () in
     let* data = Db.get_browser_sanitized_results_all_light pool in
     Fmt.pr "Samples to analyse: %d\n%!" (List.length data);
     let post_db_ts = Ts.timestamp () in
     let mode_ht = fill_mode_ht lookups.browsers data in
     let _ = analyse_mangled lookups verbose mode_ht delta_tbls in
     let post_analysis_ts = Ts.timestamp () in
     let pre_print_ts = Ts.timestamp () in
     let* () = print_stats ~tex ~csv out_dir lookups delta_tbls in
     let post_print_ts = Ts.timestamp () in
     let db_span = Ts.diff initial_ts post_db_ts in
     let analysis_span = Ts.diff post_analysis_ts post_db_ts in
     let print_span = Ts.diff pre_print_ts post_print_ts in
     let total_span = Ts.diff initial_ts post_print_ts in
     print_timings total_span db_span analysis_span print_span;
     Lwt.return ());
  `Ok ()

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let tex =
    let doc = "Print tex files" in
    Arg.(value & flag & info [ "t"; "tex" ] ~doc)
  in
  let csv =
    let doc = "Print csv files" in
    Arg.(value & flag & info [ "csv" ] ~doc)
  in
  let out_dir =
    let doc = "Output directory for tex files" in
    Arg.(value & opt dir "/tmp" & info [ "out" ] ~doc)
  in
  let config_fname =
    let doc = "Configuration file" in
    Arg.(value & opt string "config.toml" & info [ "config" ] ~doc)
  in
  let doc = "Analyse browser behavior" in
  let info = Cmd.info "browser_analysis" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const analyse $ verbose $ tex $ csv $ out_dir $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
