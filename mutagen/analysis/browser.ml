open Containers
open Mxssy
module Ts = Timings

let print_timings initial_ts post_db_ts post_analysis_ts final_ts =
  let duration_total = Ts.diff initial_ts final_ts in
  let duration_db = Ts.diff initial_ts post_db_ts in
  let duration_analysis = Ts.diff post_db_ts post_analysis_ts in
  let duration_print = Ts.diff post_analysis_ts final_ts in
  Fmt.pr "Took %a in total. Database access took %a, analysis took %a and printing took %a\n%!" Ts.pr duration_total
    Ts.pr duration_db Ts.pr duration_analysis Ts.pr duration_print

type differences = All_equal | Chrome_different | Firefox_different | Webkit_different | All_different
[@@deriving show { with_path = false }, eq, hash]

let print_diference = function
  | All_equal -> "All_equal"
  | Chrome_different -> "Chrome_different"
  | Firefox_different -> "Firefox_different"
  | Webkit_different -> "Webkit_different"
  | All_different -> "All_different"

type record = { id : int; mode : Util.Mode.t; payload : string } [@@deriving show { with_path = false }, eq, hash]

let recordings : (differences, record Bounded_counting_list.t) Hashtbl.t =
  [
    (All_equal, Bounded_counting_list.create 5);
    (All_different, Bounded_counting_list.create 5);
    (Chrome_different, Bounded_counting_list.create 5);
    (Webkit_different, Bounded_counting_list.create 5);
    (Firefox_different, Bounded_counting_list.create 5);
  ]
  |> Hashtbl.of_list

let add_recording difference record =
  let entry = Hashtbl.get recordings difference in
  match entry with
  | Some e -> Bounded_counting_list.add e record
  | None -> failwith (Fmt.str "Difference '%s' is unknown!\n%!" (print_diference difference))

let analyse_sample (id, payload, mode, chrome, chrome_s, webkit, webkit_s, firefox, firefox_s) =
  let eq (rl, sl) (rr, sr) =
    let module BE = Database.Browser_exec in
    let module BS = Database.Browser_exec_status in
    BE.equal rl rr && BS.equal sl sr
  in
  let chrome' = (chrome, chrome_s) in
  let webkit' = (webkit, webkit_s) in
  let firefox' = (firefox, firefox_s) in
  if eq chrome' webkit' && eq webkit' firefox' then add_recording All_equal { id; mode; payload }
  else if eq chrome' webkit' then add_recording Firefox_different { id; mode; payload }
  else if eq chrome' firefox' then add_recording Webkit_different { id; mode; payload }
  else if eq webkit' firefox' then add_recording Chrome_different { id; mode; payload }
  else add_recording All_different { id; mode; payload }

let print_sample name data total =
  let data' = Bounded_counting_list.content data in
  let count = Bounded_counting_list.count data in
  let perc_of = Float.of_int count /. (Float.of_int total /. 100.) in
  match data' with
  | [] -> Fmt.pr "For %s: no samples were found!\n%!" (print_diference name)
  | _ ->
      Fmt.pr "For %s %d samples were found (%f%%):\n%!" (print_diference name) count perc_of;
      data' |> List.iter (fun { id; mode; payload } -> Fmt.pr "\t%d: Mode '%a': %s\n%!" id Util.Mode.pp mode payload)

let print_samples verbose total =
  let el : record Bounded_counting_list.t = Bounded_counting_list.create 0 in
  let all_equal = Hashtbl.get_or recordings All_equal ~default:el in
  let all_different = Hashtbl.get_or recordings All_different ~default:el in
  let cdiff = Hashtbl.get_or recordings Chrome_different ~default:el in
  let wdiff = Hashtbl.get_or recordings Webkit_different ~default:el in
  let fdiff = Hashtbl.get_or recordings Firefox_different ~default:el in
  print_sample All_equal all_equal total;
  print_sample All_different all_different total;
  print_sample Chrome_different cdiff total;
  print_sample Webkit_different wdiff total;
  print_sample Firefox_different fdiff total

let analyse verbose total config_fname =
  let module Db = Database in
  let module Cfg = Config in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     (*let ( and* ) = Lwt.both in*)
     let config = Cfg.parse config_fname in
     let* pool = Db.connect config.Cfg.database in
     let initial_ts = Timings.timestamp () in
     let* data = Db.compare_evaluations pool total in
     let post_db_ts = Timings.timestamp () in
     let () = data |> List.iter analyse_sample in
     let post_analysis_ts = Timings.timestamp () in
     print_samples verbose (List.length data);
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
     let* data = Db.compare_evaluation pool id in
     let post_db_ts = Timings.timestamp () in
     let () = data |> List.iter analyse_sample in
     let post_analysis_ts = Timings.timestamp () in
     print_samples verbose (List.length data);
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
