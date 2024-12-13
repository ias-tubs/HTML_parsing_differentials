open Containers

module Lookups = struct
  type t = { browsers : (int, string) Hashtbl.t; modes : (int, string) Hashtbl.t; sanitizers : (int, string) Hashtbl.t }
  [@@deriving show { with_path = false }]
end

module Browser = struct
  type t = { id : int; name : string } [@@deriving show { with_path = false }, eq, hash]

  let cmp_name l r =
    match (l, r) with
    | "chromium", "webkit" -> -1
    | "chromium", "firefox" -> -1
    | "webkit", "firefox" -> -1
    | "webkit", "chromium" -> 1
    | "firefox", "webkit" -> 1
    | "firefox", "chromium" -> 1
    | "firefox", "firefox" -> 0
    | "chromium", "chromium" -> 0
    | "webkit", "webkit" -> 0
    | _ -> failwith @@ Fmt.str "Unknown browsers %s <-> %s\n%!" l r
end

module Mode = struct
  type t = { id : int; name : string } [@@deriving show { with_path = false }, eq, hash]

  let cmp_name l r =
    match (l, r) with
    | "document_write", "innerHTML" -> 1
    | "innerHTML", "document_write" -> -1
    | "innerHTML", "innerHTML" -> 0
    | "document_write", "document_write" -> 0
    | _ -> failwith @@ Fmt.str "Unknown modes %s <-> %s\n%!" l r
end

module Sanitizer = struct
  type t = { id : int; name : string } [@@deriving show { with_path = false }, eq, hash]
end

let encode_char = function
  | ' ' -> "%20"
  | '"' -> "%22"
  | '<' -> "%3C"
  | '>' -> "%3E"
  | '`' -> "%60"
  | '%' -> "%25"
  | '{' -> "%7B"
  | '}' -> "%7E"
  | ']' -> "%5D"
  | '[' -> "%5B"
  | c -> Char.to_string c

let encode_char_component = function
  | ';' -> "%3B"
  | ',' -> "%2C"
  | '/' -> "%2F"
  | '?' -> "%3F"
  | ':' -> "%3A"
  | '@' -> "%40"
  | '&' -> "%26"
  | '=' -> "%3D"
  | '+' -> "%2B"
  | '$' -> "%24"
  | '#' -> "%23"
  | c -> encode_char c

let encode_uri s = s |> String.to_list |> List.map encode_char |> String.concat ""
let encode_uri_component s = s |> String.to_list |> List.map encode_char_component |> String.concat ""

let encode_char_xml = function
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '&' -> "&amp;"
  | '"' -> "&quot;"
  | '\'' -> "&apos;"
  | c -> Char.to_string c

let xml_encode s = s |> String.to_list |> List.map encode_char_xml |> String.concat ""

let drop_first cmp needle xs =
  let rec drop_first' needle acc = function
    | x :: xs -> if cmp x needle then acc @ xs else drop_first' needle (acc @ [ x ]) xs
    | [] -> acc
  in
  drop_first' needle [] xs

let list_max cmp init xs = xs |> List.fold_left (fun i a -> if cmp i a > 0 then i else a) init

let make_steps l u step_size =
  let rec steps a l acc =
    if l >= u then acc
    else
      let s = (l + a, Int.min (l + step_size) u) in
      steps 1 (l + step_size) [ s ] @ acc
  in
  steps 0 l [] |> List.rev

let take_until_inc f lst =
  let rec take acc = function
    | x :: xs ->
        let acc' = [ x ] @ acc in
        if f x then acc' |> List.rev else take acc' xs
    | [] -> []
  in
  let res = take [] lst in
  res

let with_parents lst =
  let rec wp acc p = function
    | x :: xs ->
        let acc' = (x, p) :: acc in
        wp acc' (Some x) xs
    (*| [x] -> (x,p)::acc*)
    | [] -> acc
  in
  wp [] None lst |> List.rev
