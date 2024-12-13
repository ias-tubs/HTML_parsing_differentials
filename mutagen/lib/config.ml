type db_config = { hostname : string; port : int; db_name : string; username : string; password : string }
[@@deriving show { with_path = false }]

let default_db_config =
  {
    hostname = "database";
    port = 5432;
    username = "mxssy";
    password = "allyourmutationsarebelongtous";
    db_name = "mxssy";
  }

type t = { verbose : bool; database : db_config } [@@deriving show { with_path = false }]

let default_config = { verbose = true; database = default_db_config }

let parse_db_config toml =
  let hostname =
    Otoml.find_or ~default:default_db_config.hostname toml (Otoml.get_string ~strict:false)
      [ "settings"; "database"; "hostname" ]
  in
  let port =
    Otoml.find_or ~default:default_db_config.port toml (Otoml.get_integer ~strict:false)
      [ "settings"; "database"; "port" ]
  in
  let username =
    Otoml.find_or ~default:default_db_config.username toml (Otoml.get_string ~strict:false)
      [ "settings"; "database"; "username" ]
  in
  let db_name =
    Otoml.find_or ~default:default_db_config.db_name toml (Otoml.get_string ~strict:false)
      [ "settings"; "database"; "db_name" ]
  in
  let password =
    Otoml.find_or ~default:default_db_config.password toml (Otoml.get_string ~strict:false)
      [ "settings"; "database"; "password" ]
  in
  { hostname; port; username; db_name; password }

let parse fname =
  let toml = Otoml.Parser.from_file fname in
  let verbose =
    Otoml.find_or ~default:default_config.verbose toml (Otoml.get_boolean ~strict:true) [ "settings"; "verbose" ]
  in
  let database = parse_db_config toml in
  { verbose; database }
