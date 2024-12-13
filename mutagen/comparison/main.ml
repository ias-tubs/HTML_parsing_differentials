open Mxssy

let score _ id config_fname =
  let config = Config.parse config_fname in
  (*Fmt.pr "%a\n%!" Mxssy.Config.pp config;*)
  let module Db = Database in
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* pool = Db.connect config.Config.database in
     let* () = Compare.compare pool id in
     Lwt.return_unit);
  `Ok ()

let cmdline =
  let open Cmdliner in
  let verbose =
    let doc = "Give additional output" in
    Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
  in
  let id =
    let doc = "Test on $(ID)" in
    Arg.(value & opt int 3812110 & info [ "i"; "id" ] ~docv:"ID" ~doc)
  in
  let config_fname =
    let doc = "Configuration file" in
    Arg.(value & opt string "config.toml" & info [ "config" ] ~doc)
  in
  let doc = "score mxss payloads" in
  let info = Cmd.info "gen" ~version:"0.001" ~doc in
  Cmd.v info Term.(ret (const score $ verbose $ id $ config_fname))

let () = exit (Cmdliner.Cmd.eval cmdline)
