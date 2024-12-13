open Common


let iter iterate s =
  Kstream.iter iterate s (function
    | Exit -> ()
    | exn -> raise exn)
    ignore


let tokenize input =
  let report location err _ unit_rep =
    let _ = location,err in
    (*let s = Error.to_string ~location err in*)
    (*print_endline s;*)
    unit_rep ()
  in
  let stream, _, set_foreign =
    input
    |> Stream_io.string
    |> Encoding.utf_8
    |> Input.preprocess is_valid_html_char Error.ignore_errors
    |> Html_tokenizer.tokenize report
  in
  set_foreign (fun () -> false);

  let tokens = ref [] in
  let iterate (l, t) _ k  =
    tokens := (l, t) :: !tokens;
    k ()
  in
  iter iterate stream;
  let merge tokens =
    let rec merge' (acc, in_string, (sl, sc), buf) = function
      | ((ll, lc), t) as t' :: tl ->
        (begin match t with
           | `Char c ->
             if in_string then
                let buf = (char c) ^ buf in
                merge' (acc, in_string, (sl, sc), buf) tl
              else
                let buf = char c in
                merge' (acc, true, (ll, lc), buf) tl
           | `Char_ref s ->
             if in_string then
               let buf =  "&" ^ s^";" ^ buf in
                merge' (acc, in_string, (sl, sc), buf) tl
              else
                let buf = "&" ^ s^";" in
                merge' (acc, true, (ll, lc), buf) tl
          | _ -> if in_string then
              let acc' = acc @ [(sl, sc), `String(buf); t'] in
               merge' (acc', false, (0, 0), buf) tl
            else
              merge' (acc @ [t'], false, (0, 0), "") tl
        end)
      | [] -> if in_string then
          acc @ [(sl, sc), `String(buf)]
        else acc
    in
    merge' ([], false, (0,0), "") tokens in

  let compressed = merge !tokens in
  compressed |> List.rev

let tokens_to_ast input =
  let tokens = tokenize input in
  let rec convert acc = function
    | [] -> acc
    | (_,token) :: tl -> (begin
        convert ([token] @ acc) tl
        end) in
  convert [] tokens
let print_tokenz input =
  tokenize input |> List.iter( (fun ((l, c),t) ->
      let s = token_to_string t in
      print_endline (Format.sprintf "%d:%d: %s" l c s
                    )))
