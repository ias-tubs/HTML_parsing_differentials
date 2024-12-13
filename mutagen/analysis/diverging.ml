open Containers
open Mxssy
module Ts = Timings
module S = Util.Sanitizer
module B = Util.Browser
module M = Util.Mode
module E = Database.Exec

let print_timings initial_ts post_db_ts post_analysis_ts final_ts =
  let duration_total = Ts.diff initial_ts final_ts in
  let duration_db = Ts.diff initial_ts post_db_ts in
  let duration_analysis = Ts.diff post_db_ts post_analysis_ts in
  let duration_print = Ts.diff post_analysis_ts final_ts in
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr duration_total
    Ts.pr duration_db Ts.pr duration_analysis Ts.pr duration_print

let analyse_exec (id, payload, _, sanitizer, browser, (mode1, exec1), (mode2, exec2), (mode3, exec3)) =
  Fmt.pr "Diverging execution for %d ('%s') with %s/%s:\n%!" id payload browser.B.name sanitizer.S.name;
  let ast1 = Html_parser.process' exec1.E.serialized |> Result.get_exn in
  let ast2 = Html_parser.process' exec2.E.serialized |> Result.get_exn in
  let ast3 = Html_parser.process' exec3.E.serialized |> Result.get_exn in
  Fmt.pr "Mode: %s executed? %b\n%!" mode1.M.name exec1.E.executed;
  Fmt.pr "\t%a\n%!" Ast.pp ast1;
  Fmt.pr "Mode: %s executed? %b\n%!" mode2.M.name exec2.E.executed;
  Fmt.pr "\t%a\n%!" Ast.pp ast2;
  Fmt.pr "Mode: %s executed? %b\n%!" mode3.M.name exec3.E.executed;
  Fmt.pr "\t%a\n%!" Ast.pp ast3;
  ()

let analyse verbose total config_fname =
  let module Db = Database in
  let module Cfg = Config in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     (*let ( and* ) = Lwt.both in*)
     let config = Cfg.parse config_fname in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* data = Db.get_diverging_execs pool total in
     let post_db_ts = Timings.timestamp () in
     let () = data |> List.iter analyse_exec in
     let post_analysis_ts = Timings.timestamp () in
     (*print_samples verbose (List.length data);*)
     let final_ts = Timings.timestamp () in
     print_timings initial_ts post_db_ts post_analysis_ts final_ts;
     Lwt.return ());
  `Ok ()

let analyse_id verbose id config_fname =
  let module Db = Database in
  let module Cfg = Config in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     (*let ( and* ) = Lwt.both in*)
     let config = Cfg.parse config_fname in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* data = Db.get_diverging_exec pool id in
     let post_db_ts = Timings.timestamp () in
     let () = data |> List.iter analyse_exec in
     let post_analysis_ts = Timings.timestamp () in
     (*print_samples verbose (List.length data);*)
     let final_ts = Timings.timestamp () in
     print_timings initial_ts post_db_ts post_analysis_ts final_ts;
     Lwt.return ());
  `Ok ()

let analyse' verbose total id config_fname =
  match id with None -> analyse verbose total config_fname | Some i -> analyse_id verbose i config_fname

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let total =
    let doc = "Analyse $(docv) Payloads" in
    Arg.(value & opt (some int) None & info [ "c"; "count" ] ~docv:"COUNT" ~doc)
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
  Cmd.v info Term.(ret (const analyse' $ verbose $ total $ id $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
