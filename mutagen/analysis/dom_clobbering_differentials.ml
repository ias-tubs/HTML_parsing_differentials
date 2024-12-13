open Containers
open Mxssy
module Ts = Timings
module B = Util.Browser
module M = Util.Mode
module S = Util.Sanitizer
module C = Clobbering_analysis

let reference_browser = 1
let reference_mode = 1

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

let analyse_sample id reference_data sanitizer_data =
  let browser, mode, payload, _, serialized = reference_data in
  (*Fmt.pr "Analysing sample %d, comparing against %a, %a: %s/%s\n%!" id B.pp browser M.pp mode payload serialized;*)
  let ref_ast = Html_parser.process' serialized |> Result.get_exn in
  let p = Hashtbl.get_or Preprocessing.browser_postprocess browser.B.name ~default:Fun.id in
  let ref_ast = p ref_ast in
  let ref_ids = C.get_id_set ref_ast in
  let sanitizer_data =
    sanitizer_data
    |> List.filter_map (fun (sanitizer, serialized) ->
           let pp = Hashtbl.get_or Preprocessing.sanitizer_preprocess sanitizer.S.name ~default:(fun (_, p) -> p) in
           let serialized' = pp (payload, serialized) in
           let ast' = Html_parser.process' serialized' in
           match ast' with
           | Result.Error e ->
               Fmt.epr "Error parsing serialized DOM of %d for %a: %s\nDOM: %s\n%!" id S.pp sanitizer e serialized';
               None
           | Result.Ok ast ->
               let p = Hashtbl.get_or Preprocessing.sanitizer_postprocess sanitizer.S.name ~default:Fun.id in
               let ast = p ast in
               let ids = C.get_id_set ast in
               Some (sanitizer, ast, ids))
  in
  sanitizer_data
  |> List.iter (fun (sanitizer, ast, ids) ->
         if C.StringSet.cardinal ref_ids > C.StringSet.cardinal ids then (
           let delta = C.StringSet.diff ids ref_ids in
           if C.StringSet.is_empty delta then (*Fmt.pr "No missing attribute for%a\n%!" S.pp sanitizer*)
             ()
           else
             let missing = C.StringSet.to_list delta in
             missing |> List.iter (fun n -> Fmt.pr "%s is missing here!\n%!" n);
             Fmt.pr "%a is missing [%a] compared to reference\nWith parse tree:\n%a\nand reference:\n%a\n%!" S.pp
               sanitizer
               (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
               missing Ast.pp ast Ast.pp ref_ast;
             Fmt.pr "\n%s\n%!" "===========================================================")
         else ())

let sanitizer_data_tbl data =
  let tbl = Hashtbl.create (List.length data) in
  data |> List.iter (fun (id, sanitizer, _, serialized) -> Hashtbl.add_list tbl id (sanitizer, serialized));
  tbl

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
     let ( and* ) = Lwt.both in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* sanitizer_data = Db.get_sanitizer_results_range_light pool rl rr
     and* browser_data = Db.get_browser_results_range_light pool rl rr in
     let post_db_ts = Timings.timestamp () in
     let sanitizer_groups = sanitizer_data_tbl sanitizer_data in
     let browser_groups = browser_data_tbl browser_data in
     let keys = Hashtbl.keys_list browser_groups in
     let data =
       keys
       |> List.map (fun k ->
              (k, Hashtbl.get_or browser_groups ~default:[] k, Hashtbl.get_or sanitizer_groups ~default:[] k))
     in
     data
     |> List.iter (fun (key, browser_data', sanitizer_data') ->
            let reference_data =
              browser_data'
              |> List.find (fun (browser, mode, _, _, _) ->
                     Int.equal browser.B.id reference_browser && Int.equal mode.M.id reference_mode)
            in
            analyse_sample key reference_data sanitizer_data');
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
     let ( and* ) = Lwt.both in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* sanitizer_data = Db.get_sanitizer_results_light pool id and* browser_data = Db.get_browser_results pool id in
     let post_db_ts = Timings.timestamp () in
     let reference_data =
       browser_data
       |> List.find (fun (_, _, _, browser, mode, _, _, _) ->
              Int.equal browser.B.id reference_browser && Int.equal mode.M.id reference_mode)
     in
     let sanitizer_data' = sanitizer_data |> List.map (fun (s, _, serialized) -> (s, serialized)) in
     let _, payload, _, ref_browser, ref_mode, output, serialized, _ = reference_data in
     let () = analyse_sample id (ref_browser, ref_mode, output, payload, serialized) sanitizer_data' in
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
