open Containers

let id_of_tag = function
  | Ast.Tag (_, attrs, _, _) ->
      attrs |> List.find_opt (fun Ast.{ key; _ } -> String.equal_caseless key "id") |> Option.map (fun i -> i.Ast.value)
  | _ -> None

let rec map' ast f acc =
  let open Ast in
  match ast with
  | Tag (_, _, _, cs) as t ->
      let t' = f t in
      let acc' = t' :: acc in
      let x = cs |> List.map (fun c -> map' c f []) in
      acc' :: x |> List.flatten
  | t ->
      let t' = f t in
      t' :: acc

let map (ast : Ast.t) f = map' ast f []

let is_payload = function
  | Ast.(Tag ("IMG", [ { key = "src"; value = "x" }; { key = "onerror"; value = "mxss(1)" } ], _, _)) -> true
  | Ast.(Tag ("SCRIPT", [], _, [ Text "mxss(1)" ])) -> true
  | _ -> false

let count_payloads ast =
  let payloads = map ast is_payload in
  payloads |> List.count (Bool.equal true)

let has_foreign_content ast =
  let open Ast in
  let payloads =
    map ast (function
      | Tag ("SVG", _, _, _) | Tag ("MTEXT", _, _, _) | Tag ("MGLYPH", _, _, _) | Tag ("MATH", _, _, _) -> true
      | _ -> false)
  in
  payloads |> List.exists (Bool.equal true)

let unique_ids ast =
  let ids = map ast id_of_tag in
  let ids = ids |> List.keep_some in
  let id_count = List.length ids in
  let ids_dedup = List.uniq ~eq:String.equal_caseless ids in
  let id_dedup_count = List.length ids_dedup in
  Int.equal id_count id_dedup_count

(** TODO: Add the Gen.t to match against? *)
let score (ast : Ast.t) =
  let score = 0 in
  let score = if unique_ids ast then score else score + 1 in
  let score = score + count_payloads ast in
  let score = if has_foreign_content ast then score else score + 3 in
  score
