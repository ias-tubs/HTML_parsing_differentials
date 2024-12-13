open Containers

module Jaccard = struct
  module StringSet = Set.Make (struct
    type t = String.t

    let compare l r = match (l, r) with "img", "image" -> 0 | "image", "img" -> 0 | _ -> String.compare l r
  end)

  type t = StringSet.t

  let collect_benign_tags a =
    let open Ast in
    let rec collect' acc = function
      | Document children ->
          let ca = children |> List.flat_map (collect' []) in
          acc @ ca
      | Document_fragment children ->
          let ca = children |> List.flat_map (collect' []) in
          acc @ ca
      | Tag (name, _, _, children) ->
          let ca = children |> List.flat_map (collect' []) in
          if List.mem ~eq:String.equal_caseless name [ "img"; "image"; "script" ] then ca else (name :: acc) @ ca
      | Text _ -> "#text" :: acc
      | Cdata _ -> "#cdata" :: acc
      | Comment _ -> "#comment" :: acc
      | Document_type _ -> acc
    in
    collect' [] a

  let collect_tags a =
    let open Ast in
    let rec collect' acc = function
      | Document children ->
          let ca = children |> List.flat_map (collect' []) in
          acc @ ca
      | Document_fragment children ->
          let ca = children |> List.flat_map (collect' []) in
          acc @ ca
      | Tag (name, _, _, children) ->
          let ca = children |> List.flat_map (collect' []) in
          (name :: acc) @ ca
      | Text _ -> "#text" :: acc
      | Cdata _ -> "#cdata" :: acc
      | Comment _ -> "#comment" :: acc
      | Document_type _ -> acc
    in
    collect' [] a

  let from_ast_benign a = StringSet.of_list (collect_benign_tags a)
  let from_ast a = StringSet.of_list (collect_tags a)

  let similarity ?(verbose = false) ls rs =
    let union = StringSet.union ls rs in
    let unionl = StringSet.cardinal union |> Float.of_int in
    let intersection = StringSet.inter ls rs in
    let interl = StringSet.cardinal intersection |> Float.of_int in
    if Float.equal unionl interl then 1.0
    else (
      if verbose then
        Fmt.pr "%f %a/%f (%a)\n%!" unionl
          (StringSet.pp ~pp_start:(Fmt.any "(") ~pp_stop:(Fmt.any ")") ~pp_sep:(Fmt.any ", ") String.pp)
          union interl
          (StringSet.pp ~pp_start:(Fmt.any "(") ~pp_stop:(Fmt.any ")") ~pp_sep:(Fmt.any ", ") String.pp)
          intersection
      else ();
      interl /. unionl)

  let distance ?(verbose = false) ls rs = 1.0 -. similarity ~verbose ls rs

  let similarity' ?(verbose = false) l r =
    let ls = from_ast l in
    let rs = from_ast r in
    similarity ~verbose ls rs

  let distance' ?(verbose = false) l r = 1.0 -. similarity' ~verbose l r
end

(** https://dl.acm.org/doi/pdf/10.1145/956750.956822 *)
module BagOfXPaths = struct
  type path_node = Root | Text of int | CData of int | Comment of int | Tag of string * int
  [@@deriving show { with_path = false }, hash, eq]

  type path = path_node list [@@deriving show { with_path = false }, hash, eq]

  module XPaths = CCHashSet.Make (struct
    type t = path

    let equal l r = equal_path l r
    let hash p = hash_path p
  end)

  type t = XPaths.t

  let fmt_node pf = function
    | Root -> Fmt.pf pf "%s" "root"
    | Text c -> Fmt.pf pf "text[%d]" c
    | CData c -> Fmt.pf pf "cdata[%d]" c
    | Comment c -> Fmt.pf pf "comment[%d]" c
    | Tag (n, c) -> Fmt.pf pf "%s[%d]" n c

  let fmt pf path = Fmt.pf pf "%a" (Fmt.list ~sep:(Fmt.any "/") fmt_node) path

  let pp pf xpaths =
    let paths = XPaths.elements xpaths in
    Fmt.pf pf "%a" (Fmt.list ~sep:(Fmt.any ", ") fmt) paths

  type counter = { tags : (string, int) Hashtbl.t; cdata : int ref; comment : int ref; text : int ref }

  let make_counter () = { tags = Hashtbl.create 5; cdata = ref 0; comment = ref 0; text = ref 0 }

  let make_node counter ast_node =
    let module A = Ast in
    match ast_node with
    | A.Tag (name, _, _, _) ->
        let count = Hashtbl.get_or counter.tags name ~default:0 in
        Hashtbl.incr ~by:1 counter.tags name;
        Tag (name, count)
    | A.Text _ ->
        let count = !(counter.text) in
        counter.text := !(counter.text) + 1;
        Text count
    | A.Cdata _ ->
        let count = !(counter.cdata) in
        counter.cdata := !(counter.cdata) + 1;
        CData count
    | A.Comment _ ->
        let count = !(counter.comment) in
        counter.comment := !(counter.comment) + 1;
        Comment count
    | _ -> Root

  let from_ast_benign ast =
    let rec from_ast' acc cnt ast =
      let module A = Ast in
      match ast with
      | A.Document cs | A.Document_fragment cs ->
          (*Fmt.pr "Document: %a\n%!" A.pp ast;*)
          let acc = [ Root ] in
          let cs' = cs |> List.flat_map (fun c -> from_ast' acc cnt c) in
          cs'
      | A.Tag (n, _, _, []) as cur ->
          (*Fmt.pr "Leaf Tag: %a\n%!" A.pp ast;*)
          if List.mem ~eq:String.equal_caseless n [ "img"; "image"; "script" ] then [ acc ]
          else
            let node = make_node cnt cur in
            [ acc @ [ node ] ]
      | A.Tag (n, _, _, cs) as cur ->
          (*Fmt.pr "Tag: %a\n%!" A.pp ast;*)
          if List.mem ~eq:String.equal_caseless n [ "img"; "image"; "script" ] then [ acc ]
          else
            let node = make_node cnt cur in
            let acc = acc @ [ node ] in
            let cnt = make_counter () in
            let cs' = cs |> List.flat_map (fun c -> from_ast' acc cnt c) in
            cs'
      | (A.Cdata _ | A.Text _ | A.Comment _) as cur ->
          (*Fmt.pr "Node: %a\n%!" A.pp ast;*)
          let node = make_node cnt cur in
          [ acc @ [ node ] ]
      | _ -> []
    in
    let cnt = make_counter () in
    from_ast' [] cnt ast |> XPaths.of_list

  let from_ast ast =
    let rec from_ast' acc cnt ast =
      let module A = Ast in
      match ast with
      | A.Document cs | A.Document_fragment cs ->
          (*Fmt.pr "Document: %a\n%!" A.pp ast;*)
          let acc = [ Root ] in
          let cs' = cs |> List.flat_map (fun c -> from_ast' acc cnt c) in
          cs'
      | A.Tag (n, _, _, []) as cur ->
          (*Fmt.pr "Leaf Tag: %a\n%!" A.pp ast;*)
          let node = make_node cnt cur in
          [ acc @ [ node ] ]
      | A.Tag (n, _, _, cs) as cur ->
          (*Fmt.pr "Tag: %a\n%!" A.pp ast;*)
          let node = make_node cnt cur in
          let acc = acc @ [ node ] in
          let cnt = make_counter () in
          let cs' = cs |> List.flat_map (fun c -> from_ast' acc cnt c) in
          cs'
      | (A.Cdata _ | A.Text _ | A.Comment _) as cur ->
          (*Fmt.pr "Node: %a\n%!" A.pp ast;*)
          let node = make_node cnt cur in
          [ acc @ [ node ] ]
      | _ -> []
    in
    let cnt = make_counter () in
    from_ast' [] cnt ast |> XPaths.of_list

  let similarity ?(verbose = false) l r =
    let c = XPaths.inter l r in
    let cl = XPaths.cardinal c in
    let ll = XPaths.cardinal l in
    let rl = XPaths.cardinal r in
    if verbose then Fmt.pr "%a & %a -> %a\n%!" pp l pp r pp c else ();
    if Int.equal cl 0 && Int.equal (ll + rl) 0 then 1.
    else
      let cs = Float.of_int cl in
      let ls = Float.of_int ll in
      let rs = Float.of_int rl in
      cs /. (ls +. rs -. cs)

  let similarity' ?(verbose = false) l r =
    let l' = from_ast l in
    let r' = from_ast r in
    similarity ~verbose l' r'

  let distance ?(verbose = false) l r = 1.0 -. similarity ~verbose l r

  let distance' ?(verbose = false) l r =
    let l' = from_ast l in
    let r' = from_ast r in
    distance ~verbose l' r'
end
