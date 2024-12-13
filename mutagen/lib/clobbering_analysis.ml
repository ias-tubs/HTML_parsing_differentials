open Containers

type gadget = Multiple_Ids of (string * int) | Multiple_Names of (string * int)
[@@deriving show { with_path = false }, eq, hash]

module StringSet = Set.Make (struct
  type t = String.t

  let compare l r = String.compare l r
end)

module Clobbering = struct
  type t = {
    id : int;
    browser : Util.Browser.t;
    mode : Util.Mode.t;
    payload : string;
    result : string;
    potentials : gadget list;
  }
  [@@deriving show { with_path = false }, eq, hash]
end

let get_ids_and_names (sample : Ast.t) =
  let ids = Hashtbl.create 10 in
  let names = Hashtbl.create 10 in
  let open Ast in
  let inc attr =
    match attr.Ast.key with
    | "id" -> Hashtbl.incr ~by:1 ids attr.value
    | "name" -> Hashtbl.incr ~by:1 names attr.value
    | _ -> ()
  in
  let rec collect' = function
    | Document_fragment children | Document children ->
        let () = children |> List.iter collect' in
        ()
    | Tag (name, attrs, _, children) ->
        let () = attrs |> List.iter inc in
        let () = children |> List.iter collect' in
        ()
    | Text _ -> ()
    | Cdata _ -> ()
    | Comment _ -> ()
    | Document_type _ -> ()
  in
  collect' sample;
  let ids = Hashtbl.to_list ids in
  let names = Hashtbl.to_list names in
  (ids, names)

let get_ids_and_names_sorted sample =
  let ids, names = get_ids_and_names sample in
  let ids = ids |> List.fast_sort (fun (_, l) (_, r) -> Int.compare l r) in
  let names = names |> List.fast_sort (fun (_, l) (_, r) -> Int.compare l r) in
  (ids, names)

let get_id_set sample =
  let ids, _ = get_ids_and_names_sorted sample in
  let idns = ids |> List.map (fun (n, _) -> n) in
  StringSet.of_list idns

let analyse_clobbering (sample : Ast.t) =
  let ids, names = get_ids_and_names sample in
  let mids = ids |> List.filter (fun (_, cnt) -> cnt > 1) |> List.map (fun (n, cnt) -> Multiple_Ids (n, cnt)) in
  let mnames = names |> List.filter (fun (_, cnt) -> cnt > 1) |> List.map (fun (n, cnt) -> Multiple_Ids (n, cnt)) in
  mids @ mnames
