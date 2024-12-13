open Containers

let process' (line : string) =
  try
    let linebuf = Lexing.from_string line in
    let ast = Parser.main Lexer.token linebuf in
    ast |> Result.of_opt
  with ex ->
    Fmt.epr "Parsing error (%s) in %s\n%!" (Printexc.to_string ex) line;
    Result.Error (Fmt.str "Parsing error in %s\n%!" line)

let process (line : string) =
  try
    let ast = process' line in
    ast |> Result.get_exn |> Ast.show |> Fmt.pr "%s\n%!"
  with Lexer.SyntaxError msg -> Fmt.epr "%s%!" msg

let process (optional_line : string option) = match optional_line with None -> () | Some line -> process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then repeat channel

let parse = process
let run () = repeat (Lexing.from_channel stdin)
