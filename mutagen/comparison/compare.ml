open Containers
open Mxssy
module Db = Database

module Browser = struct
  type t = { id : int; name : string } [@@deriving show { with_path = false }, eq, hash]
end

module Sanitizer = struct
  type t = { id : int; name : string } [@@deriving show { with_path = false }, eq, hash]
end

module B = Browser
module S = Sanitizer

let fmt_parent pff = function
  | Ast.Text _ -> Fmt.pf pff "text"
  | Ast.Comment _ -> Fmt.pf pff "comment"
  | Ast.Cdata _ -> Fmt.pf pff "comment"
  | Ast.Tag (name, _, _, _) -> Fmt.pf pff "%s" name
  | Ast.Document_fragment _ -> Fmt.pf pff "%s" "document-fragment"
  | Ast.Document _ -> Fmt.pf pff "%s" "document"
  | Ast.Document_type t -> Fmt.pf pff "document_type %s" t

let compare pool id =
  let ( let* ) = Lwt.bind in
  let* payload, pretty = Db.get_generation pool id in
  Fmt.pr "%s (%s)\n%!" payload pretty;
  let ast = Ast.of_string (Fmt.str "<div id=\"elem\">%s</div>" payload) in
  (*Fmt.pr "%a\n%!" (Fmt.list ~sep:(Fmt.any "; ") Ast.pp) ast;*)
  let payload_depth = Ast.payload_depths ast in
  let _ =
    match payload_depth with
    | None -> Fmt.pr "no payload found - say whaat?"
    | Some (i, ps) -> Fmt.pr "Found payload at depth %d: %a\n%!" i (Fmt.list ~sep:(Fmt.any ", ") fmt_parent) ps
  in
  let* evaluations = Db.get_evaluations pool id in
  let evaluations =
    evaluations
    |> List.map (fun ((bname, bid), (sname, sid), (result, serialized, executed)) ->
           (B.{ name = bname; id = bid }, S.{ name = sname; id = sid }, result, serialized, Int.equal executed 1))
  in
  let evaluations =
    evaluations
    |> List.map (fun (b, s, result, serialized, executed) ->
           let ast = Html_parser.process' serialized |> Result.get_exn in
           (b, s, result, serialized, ast, executed))
  in
  let groups =
    evaluations
    |> List.group_by
         ?hash:(Some (fun (b, _, _, _, _, _) -> b.B.id))
         ?eq:(Some (fun (bl, _, _, _, _, _) (br, _, _, _, _, _) -> Int.equal br.B.id bl.B.id))
  in
  groups
  |> List.iter
       (List.iter (fun (b, s, _, _, ast, executed) ->
            let payload_depth = Ast.payload_depth ast in
            let d, ps = match payload_depth with None -> (-1, None) | Some (i, ps) -> (i, Some ps) in
            Fmt.pr "%s-%s: found payload at depth %d. Executed: %b\t %a\n%!" b.B.name s.S.name d executed
              (Fmt.option (Fmt.list ~sep:(Fmt.any ", ") fmt_parent))
              ps
            (*Fmt.pr "%s-%s: %a\n%!" b.B.name s.S.name Ast.pp ast;*)));
  Lwt.return_unit
(*evaluations |> List.iter (fun ((bname, bid), (sname, sid), (result, serialized, executed)) -> print_endline (Fmt.str "%s:%d %s:%d: %s -> %s == %d" bname bid sname sid result serialized executed))*)
