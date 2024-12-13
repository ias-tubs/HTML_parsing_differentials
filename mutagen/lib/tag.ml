type t =
  | Div
  | Span
  (* Basic *)
  (*| Hr*)
  | Li
  | Ul
  (*| Bold*)
  | Pre
  | Var
  (* Filler *)
  (* Interesting to mutate out of *)
  | Form
  (* There can only be one *)
  | Dfn
  | Header
  (* Can't be nested *)
  | Paragraph
  | Break
  (* Layout *)
  | Link
  | Title
  | Textarea
  | Xmp
  | Iframe
  | Noembed
  | Noframes
  | Frameset
  | Plaintext
  | Noscript
  | Style
  | Script
  | Option
  (* Interesting interaction with its content, e.g., text only *)
  | Table
  | Td
  | Tr
  | Colgroup
  (* Mutates by specification *)
  | Svg
  | ForeignObject
  | Desc
  | Path
  | Math
  | Mtext
  | Mglyph
  | Mi
  | Mo
  | Mn
  | Ms
  | AnnotationXml
  | AnnotationXml_text
  | AnnotationXml_application
  (* Foreign content *)
  (* TODO: Add annotation-xml: https://www.w3.org/Math/draft-spec/chapter5.html#mixing.elements.annotation.xml *)
  | Object
  | Embed
  | Select
  | Input
  | Keygen
  | Listing
  (* All around strange *)
  | Dt
  (* Funky in conjunction with Dl *)
  | Dl
  | Font (* Common between HTML and svg *)
(* Interesting due to being self closing *)
[@@deriving show { with_path = false }, hash, yojson]

let tag = function
  | "div" -> Div
  | "span" -> Span
  | "title" -> Title
  | "form" -> Form
  | "dfn" -> Dfn
  | "header" -> Header
  | "p" -> Paragraph
  | "br" -> Break
  | "a" -> Link
  | "style" -> Style
  | "table" -> Table
  | "td" -> Td
  | "tr" -> Tr
  | "colgroup" -> Colgroup
  | "svg" -> Svg
  | "foreignobject" -> ForeignObject
  | "desc" -> Desc
  | "path" -> Path
  | "math" -> Math
  | "mtext" -> Mtext
  | "mglyph" -> Mglyph
  | "mi" -> Mi
  | "mo" -> Mo
  | "mn" -> Mn
  | "ms" -> Ms
  | "annotation-xml" -> AnnotationXml
  (* TODO: This is insufficient, can maybe be handled down the line? *)
  | "noscript" -> Noscript
  | "select" -> Select
  | "input" -> Input
  | "textarea" -> Textarea
  | "keygen" -> Keygen
  | "xmp" -> Xmp
  | "noembed" -> Noembed
  | "listing" -> Listing
  | "option" -> Option
  | "li" -> Li
  | "ul" -> Ul
  | "pre" -> Pre
  | "var" -> Var
  | "dl" -> Dl
  | "dt" -> Dt
  | "iframe" -> Iframe
  | "noframes" -> Noframes
  | "frameset" -> Frameset
  | "plaintext" -> Plaintext
  | "script" -> Script
  | "font" -> Font
  | "object" -> Object
  | "embed" -> Embed
  | _ -> failwith "unknown tag"
(* TODO: Throwing up here probably insufficient? *)
(*| "hr" -> Hr*)
(*| "b" -> Bold*)

let name = function
  | Div -> "div"
  | Span -> "span"
  | Title -> "title"
  | Form -> "form"
  | Dfn -> "dfn"
  | Header -> "header"
  | Paragraph -> "p"
  | Break -> "br"
  | Link -> "a"
  | Style -> "style"
  | Table -> "table"
  | Td -> "td"
  | Tr -> "tr"
  | Colgroup -> "colgroup"
  | Svg -> "svg"
  | ForeignObject -> "foreignobject"
  | Desc -> "desc"
  | Path -> "path"
  | Math -> "math"
  | Mtext -> "mtext"
  | Mglyph -> "mglyph"
  | Mi -> "mi"
  | Mo -> "mo"
  | Mn -> "mn"
  | Ms -> "ms"
  | AnnotationXml -> "annotation-xml"
  | AnnotationXml_text -> "annotation-xml"
  | AnnotationXml_application -> "annotation-xml"
  | Noscript -> "noscript"
  | Select -> "select"
  | Input -> "input"
  | Textarea -> "textarea"
  | Keygen -> "keygen"
  | Xmp -> "xmp"
  | Noembed -> "noembed"
  | Listing -> "listing"
  | Li -> "li"
  | Ul -> "ul"
  | Pre -> "pre"
  | Var -> "var"
  | Dl -> "dl"
  | Dt -> "dt"
  | Iframe -> "iframe"
  | Noframes -> "noframes"
  | Frameset -> "frameset"
  | Plaintext -> "plaintext"
  | Script -> "script"
  | Font -> "font"
  | Option -> "option"
  | Object -> "object"
  | Embed -> "embed"
(*| Hr -> "hr"*)
(*| Bold -> "b"*)

let pf pf n = Fmt.pf pf "%s" @@ name n

module Kind = struct
  type t = Texual | Content | Comment | Html_Svg_Transition | Html_MathMl_Transition
  [@@deriving show { with_path = false }, hash, yojson]

  let kind = function Style -> [ Texual ] | _ -> []
end

let self_closing_tags =
  [
    "area";
    "base";
    "br";
    "col";
    "embed";
    "hr";
    "img";
    "input";
    "link";
    "meta";
    "param";
    "source";
    "track";
    "wbr";
    "command";
    "keygen";
    "menuitem";
  ]
