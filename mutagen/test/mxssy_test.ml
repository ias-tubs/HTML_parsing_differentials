open Containers

let inputs =
  [
    {|(#tag DIV [(#attr "id" "elem")] [(#tag SPAN [] [(#tag KEYGEN [] []), (#tag KEYGEN [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] [])])])|};
    {| (#tag DIV [(#attr "id" "elem")] [(#text "*/"), (#tag BR [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag BR [] [])])  |};
    {| (#tag DIV [(#attr "id" "elem")] [(#text "--!>"), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#comment "<mglyph>")]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag svg [] [(#text "/*\"")]), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag SELECT [] [(#text "*/")])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag svg [] [(#tag svg [] [(#text "'"), (#tag style [] [(#tag noscript [] [])])])]), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#text "/*"), (#tag INPUT [] []), (#tag SELECT [] [])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag P [(#attr "foo" "<a><br><textarea><textarea><p foo="), (#attr "<" ""), (#attr "p" "")] [(#text "/*"), (#tag MGLYPH [] [(#tag svg [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#text "'*/'>"), (#tag BR [] []), (#text "'>"), (#tag STYLE [] [])])])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag MGLYPH [] [(#text "--!>"), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag DIV [] [(#tag DIV [] [])])])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag INPUT [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#text "*/")]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag math [] [(#tag mglyph [] [(#tag mtext [(#attr "id" "<mtext")] [(#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#text "/*>")])])])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag BR [] []), (#text "/*"), (#tag svg [] [(#tag form [(#attr "foo" "<!--<span title="), (#attr "<" ""), (#attr "noscript" "")] [(#tag noscript [] [])])]), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag MGLYPH [] [(#comment "</noscript>'></noscript>></svg>*/<!")])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#text "/*"), (#tag KEYGEN [] []), (#tag BR [] []), (#tag KEYGEN [(#attr "foo" "</style")] []), (#tag DIV [(#attr "foo" "<style")] [(#tag FORM [] [(#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] [])]), (#tag STYLE [] [(#text "><p>><!--</mglyph></keygen>")])])]) |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag BR [] []), (#tag BR [(#attr "foo" "<br title="), (#attr "<form" "")] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#text "'>'>/*"), (#tag BR [] [])])  |};
    {| (#tag DIV [(#attr "id" "elem")] [(#tag svg [(#attr "foo" "<!--<svg"), (#attr "title" "<div title=<a><span id="), (#attr "<!--<mtext" "")] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag BR [] []), (#text "-->>>'>\"*/"), (#tag math [] [(#text ">")])])  |};
    {|(#tag DIV [(#attr "id" "elem")] [(#tag SPAN [(#attr "id" "elem")] [(#tag KEYGEN [(#attr "id" "elem")] []), (#tag KEYGEN [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] [])])])|};
    {|(#tag DIV [(#attr "id" "elem")] [(#tag SPAN [(#attr "id" "elem")] [(#tag KEYGEN [(#attr "id" "elem")] []), (#tag KEYGEN [] []), (#tag IMG [(#attr "src" "x"), (#attr "onerror" "mxss(1)")] [])])])|};
    {|(#tag SCRIPT [] [(#text "mxss(1)")])|};
  ]

let () =
  let open Mxssy in
  inputs
  |> List.iter (fun inp ->
         let ast = Html_parser.process' inp in
         let score = Score.score ast in
         Printf.printf "%s -> %d\n%!"
           (ast |> Ast.show |> String.lines |> List.map String.trim |> String.concat " ")
           score)
