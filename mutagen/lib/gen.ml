open Containers
module IntSet = Set.Make (Int)

type state = { rng : Random.state; verbose : bool }
(** The state passed from the outside *)

(** Initializes a default {!type:state} *)
let init_state verbose = { rng = Random.get_state (); verbose }

type generation_state = {
  open_tag_count : int ref;
  open_xml_comment_count : int ref;
  open_js_comment_count : int ref;
  open_cdata_comment_count : int ref;
  uri_encoding_count : int ref;
  xml_encoding_count : int ref;
  xml_comment_count : int ref;
  js_comment_count : int ref;
  cdata_count : int ref;
  state : state;
}
(** Keeps the state of the current generation.

    Allows to decrease the likeliehood of certain Actions being taken too often.
*)

(** Converts the {!type:state} [st] to a {!generation_state} *)
let from_state (st : state) =
  {
    open_tag_count = ref 0;
    open_xml_comment_count = ref 0;
    open_js_comment_count = ref 0;
    open_cdata_comment_count = ref 0;
    uri_encoding_count = ref 0;
    xml_encoding_count = ref 0;
    xml_comment_count = ref 0;
    js_comment_count = ref 0;
    cdata_count = ref 0;
    state = st;
  }

(** Helper to automatically pass the rng state to [gen] functions. *)
let gen' f s = f s.state.rng

(** Reduces the likeliehood of a probability by a factor of c*10 *)
let decr p c = Stdlib.Float.pow p (Float.of_int (c + 1))

(** Controls the final position of an action. *)
module Placement = struct
  (** Controls where tn the resulting payload the current action should have an effect. *)
  type t = Prepend  (** In front of the resulting payload. *) | Append  (** At the end of the resulting payload. *)
  [@@deriving show { with_path = false }, hash, yojson]

  (** Generate a {!type:t} with equal probability. *)
  let gen = gen' @@ Choice.choose_uniform [ Prepend; Append ]
end

(** XSS Trigger (i.e., the actual payload) *)
module Payload = struct
  (** {!type:t} encompasses different kinds of XSS trggers. *)
  type t =
    | Img_tag  (** A [<img>] tag, i.e., the most common one. *)
    | Image_tag  (** A [<image>] tag

                           It should be parsed the same as {!Img_tag} *)
    | Script_tag  (** A [<script>] tag. *)
  [@@deriving show { with_path = false }, hash, yojson]

  (** Formats a {!type:t} for further evaluation. *)
  let print = function
    | Img_tag -> "<img src=x onerror=mxss(1)>"
    | Image_tag -> "<image src=x onerror=mxss(1)>"
    | Script_tag -> "<script>mxss(1)</script>"

  (** Generates a XSS trigger.

      This is heavily biased towards {!Img_tag} as it is the most Interesting case. *)
  let gen = gen' @@ Choice.choose_weighted [ (Img_tag, 0.6); (Image_tag, 0.2); (Script_tag, 0.2) ]
end

(** Encoding *)
module Encoding = struct
  type t = Unencoded | Xml_encoded [@@deriving show { with_path = false }, hash, yojson]
  let gen = gen' @@ Choice.choose_uniform [ Unencoded; Xml_encoded ]

end
(** Quotes are part of HTML too! *)
module Quote = struct
  (** Different kinds of quotes. *)
  type t = Unquoted | Backtick of Encoding.t | Single of Encoding.t | Double of Encoding.t [@@deriving show { with_path = false }, hash, yojson]

  let pf pf = function
    | Unquoted -> Fmt.pf pf "%s" ""
    | Backtick(Encoding.Unencoded) -> Fmt.pf pf "%s" "`"
    | Backtick(Encoding.Xml_encoded) -> Fmt.pf pf "%s" "&grave;"
    | Single(Encoding.Unencoded) -> Fmt.pf pf "%s" "'"
    | Single(Encoding.Xml_encoded) -> Fmt.pf pf "%s" "&apos;"
    | Double(Encoding.Unencoded) -> Fmt.pf pf "%s" "\""
    | Double(Encoding.Xml_encoded) -> Fmt.pf pf "%s" "&quot;"

  let gen = gen' @@ Choice.choose_uniform [ Unquoted; Backtick(Encoding.Unencoded); Single(Encoding.Unencoded); Double(Encoding.Unencoded) ]
  let gen_attribute = gen' @@ Choice.choose_weighted [ (Single(Encoding.Unencoded), 0.40); (Single(Encoding.Xml_encoded), 0.05); (Double(Encoding.Unencoded), 0.40) ; (Double(Encoding.Xml_encoded), 0.05); (Backtick(Encoding.Unencoded), 0.05) ; (Backtick(Encoding.Xml_encoded), 0.05)]
  let gen_toplevel = gen' @@ Choice.choose_weighted [ (Single(Encoding.Unencoded), 0.4); (Single(Encoding.Xml_encoded), 0.1); (Double(Encoding.Unencoded), 0.4) ; (Double(Encoding.Xml_encoded), 0.1)]
end

module Quoted = struct
  type t = Unquoted | Mixed of Quote.t * Quote.t | Front of Quote.t | Back of Quote.t | Enclosed of Quote.t
  [@@deriving show { with_path = false }, hash, yojson]

  let gen =
    gen'
    @@ Choice.choose_weighted
         [
           (Unquoted, 0.5);
           (Mixed (Quote.Unquoted, Quote.Unquoted), 0.25);
           (Front Quote.Unquoted, 0.25);
           (Back Quote.Unquoted, 0.25);
           (Enclosed Quote.Unquoted, 1.0);
         ]

  let gen_for_attr state =
    let q = gen state in
    let q' =
      match q with
      | Unquoted -> q
      | Mixed (_, _) ->
          let lhs = Quote.gen_attribute state in
          let rhs = Quote.gen_attribute state in
          Mixed (lhs, rhs)
      | Front _ ->
          let q = Quote.gen_attribute state in
          Front q
      | Back _ ->
          let q = Quote.gen_attribute state in
          Back q
      | Enclosed _ ->
          let q = Quote.gen_attribute state in
          Enclosed q
    in
    q'

  let print acc = function
    | Unquoted -> acc
    | Mixed (l, r) ->
        Fmt.str "%a%s%a" Quote.pf l acc Quote.pf r
    | Front q ->
        Fmt.str "%a%s" Quote.pf q acc
    | Back q ->
        Fmt.str "%s%a" acc Quote.pf q
    | Enclosed q ->
        Fmt.str "%a%s%a" Quote.pf q acc Quote.pf q
end

module Attribute = struct
  type form = Regular | Space_after_equals | Slash [@@deriving show { with_path = false }, hash, yojson]
  type attr = Title | Name | Id | Foo | Data_foo [@@deriving show { with_path = false }, hash, yojson]

  type t = attr * form * Quoted.t [@@deriving show { with_path = false }, hash, yojson]

  let pf_attr pf = function
    | Title -> Fmt.pf pf "%s" "title"
    | Name -> Fmt.pf pf "%s" "name"
    | Id -> Fmt.pf pf "%s" "id"
    | Foo -> Fmt.pf pf "%s" "foo"
    | Data_foo -> Fmt.pf pf "%s" "data-foo"

  let pr_attr value = function
    | (an,Regular,q) -> 
      let q = Quoted.print value q in
      Fmt.str " %a=%s" pf_attr an q
    | (an,Space_after_equals,q) -> 
      let q = Quoted.print value q in
      Fmt.str " %a= %s" pf_attr an q
    | (an,Slash,q) -> 
      let q = Quoted.print value q in
      Fmt.str "/%a=%s" pf_attr an q

  let gen_name = gen' @@ Choice.choose_uniform [ Id; Title; Foo; Name; Data_foo ]
  let gen_form = gen' @@ Choice.choose_weighted [ (Regular, 0.9); (Space_after_equals, 0.05); (Slash, 0.05)]

  let gen state =
    let a = gen_name state in
    let f = gen_form state in
    let q = Quoted.gen_for_attr state in
    (a,f,q)
end

module Attributes = struct
  type value = string [@@deriving show { with_path = false }, hash, yojson]
  type attr = Attribute.t * value [@@deriving show { with_path = false }, hash, yojson]
  type t = attr list  [@@deriving show { with_path = false }, hash, yojson]


  let pf pf attrs =
    let attrs = attrs |> List.map (fun (a,v) -> Attribute.pr_attr v a) in
    Fmt.pf pf "%a" (Fmt.list ~sep:(Fmt.any "") Fmt.string) attrs

  let gen_value = gen' @@ Choice.choose_uniform [ "foo"; "bar"; "baz"; "abc"; "xyz" ]
  let gen_attr state =
    let a = Attribute.gen state in
    let v = gen_value state in
    (a,v)
  let gen_up_to m state =
  let weights = [(0, 1.0); (1, 0.5); (2, 0.25); (3, 0.125); (4, 0.0625)] |> List.take m in
    let count = gen' (Choice.choose_weighted weights) state  in
    let attrs = List.init count (fun _ -> gen_attr state) in
    attrs
  let gen state =
    gen_up_to 3 state
  let gen_lp state =
    let count = gen' (Choice.choose_weighted [(0, 1.0); (1, 0.05); (2, 0.0025); (3, 0.000125); (4, 0.0000625)]) state  in
    let attrs = List.init count (fun _ -> gen_attr state) in
    attrs

end

module Tag = struct
  include Tag

  let pf_closing pf = function
    | t ->
        let n = name t in
        Fmt.pf pf "</%s>" n


  let pf_self_closing_tag pf = function
    | AnnotationXml_text ->
        let n = "annotation-xml encoding=\"text/html\"" in
        Fmt.pf pf "<%s/>" n
    | AnnotationXml_application ->
        let n = "annotation-xml encoding=\"application/xhtml+xml\"" in
        Fmt.pf pf "<%s/>" n
    | t ->
        let n = name t in
        Fmt.pf pf "<%s/>" n

  let pf_tag pf = function
    | AnnotationXml_text ->
        let n = "annotation-xml encoding=\"text/html\"" in
        Fmt.pf pf "<%s>" n
    | AnnotationXml_application ->
        let n = "annotation-xml encoding=\"application/xhtml+xml\"" in
        Fmt.pf pf "<%s>" n
    | t ->
        let n = name t in
        Fmt.pf pf "<%s>" n

  let gen =
    gen'
    @@ Choice.choose_weighted
         [
           (Div, 1.0);
           (Span, 1.0);
           (Title, 1.0);
           (Form, 1.0);
           (Dfn, 1.0);
           (Header, 1.0);
           (Paragraph, 0.5);
           (Break, 0.5);
           (Link, 1.0);
           (Style, 1.0);
           (Noscript, 1.0);
           (Table, 0.25);
           (Td, 0.25);
           (Tr, 0.25);
           (Colgroup, 0.25);
           (Svg, 1.0);
           (ForeignObject, 1.0);
           (Desc, 1.0);
           (Path, 1.0);
           (Math, 1.0);
           (Mtext, 0.5);
           (Mglyph, 0.5);
           (Mi, 0.25);
           (Mo, 0.25);
           (Mn, 0.25);
           (Ms, 0.25);
           (AnnotationXml, 0.33);
           (AnnotationXml_text, 0.33);
           (AnnotationXml_application, 0.33);
           (Select, 1.0);
           (Input, 1.0);
           (Option, 1.0);
           (Textarea, 1.0);
           (Keygen, 1.0);
           (Xmp, 1.0);
           (Noembed, 1.0);
           (Listing, 1.0);
           (*(Hr, 1.0);*)
           (Li, 0.5);
           (Ul, 0.5);
           (*(Bold, 1.0);*)
           (Pre, 1.0);
           (Var, 1.0);
           (Dl, 0.5);
           (Dt, 0.5);
           (Font, 1.0);
           (Plaintext, 1.0);
           (Noframes, 1.0);
           (*(Script, 1.0);*)
           (Iframe, 1.0);
           (Object, 0.5);
           (Embed, 0.5);
           (Frameset, 0.5);
         ]
end

(** Bracket type *)
module Bracket_type = struct
  type t = Opening | Closing [@@deriving show { with_path = false }, hash, yojson]
end
(** A tag can be Self_closing or not *)
module Closing_mode = struct
  type t = Self_closing | Unclosed [@@deriving show { with_path = false }, hash, yojson]
end

(** XML comments come in different forms, despite not all being valid in HTML *)
module Xml_comment_type = struct
    type t = No_bang | Bang [@@deriving show { with_path = false }, hash, yojson]
end

(** Semantic actions to take *)
module Action = struct
  type t =
    | Enclose_tag
    | Enclose_tag_attribute of Closing_mode.t
    | Self_closing_tag
    | Open_tag
    | Close_tag
    | Enclose_JavaScript_comment
    | Open_JavaScript_comment
    | Close_JavaScript_comment
    | EncodeURI_component
    | EncodeURI
    | Xml_Encode
    | Open_XML_comment
    | Close_XML_comment of Xml_comment_type.t
    | Enclose_XML_comment of Xml_comment_type.t
    | Parsing_directive
    | Angle_bracket of Bracket_type.t
    | Begin_CDATA
    | End_CDATA
    | Enclose_CDATA
    | Quote
    | Space
    | Terminate
  [@@deriving show { with_path = false }, hash, yojson]

  let closing_operation_weight max factor =
    let factor = Float.of_int factor in
    let weight = 0.1 *. factor in
    Float.max max weight

  let gen state =
    Choice.choose_weighted
      [
        (* Build some markup *)
        (Open_tag, 1.0);
        (Self_closing_tag, 1.0);
        (Enclose_tag, 1.0);
        (Enclose_tag_attribute Unclosed, 0.5);
        (Enclose_tag_attribute Self_closing, 0.25);
        (Close_tag, closing_operation_weight 1.0 !(state.open_tag_count));
        (* Interesting operations which are less interesting the more often they are chosen *)
        (Open_XML_comment, decr 0.125 !(state.xml_comment_count));
        (Close_XML_comment Bang, decr 0.125 !(state.xml_comment_count));
        (Close_XML_comment No_bang, decr 0.125 !(state.xml_comment_count));
        (Enclose_XML_comment No_bang, decr 0.125 !(state.xml_comment_count));
        (Enclose_XML_comment Bang, decr 0.125 !(state.xml_comment_count));
        (Enclose_JavaScript_comment, decr 0.01 !(state.js_comment_count));
        (Open_JavaScript_comment, decr 0.005 !(state.js_comment_count));
        (Close_JavaScript_comment, decr 0.005 !(state.js_comment_count));
        (EncodeURI_component, decr 0.0005 !(state.uri_encoding_count));
        (EncodeURI, decr 0.0001 !(state.uri_encoding_count));
        (Xml_Encode, decr 0.025 !(state.xml_encoding_count));
        (Enclose_CDATA, decr 0.05 !(state.cdata_count));
        (Begin_CDATA, decr 0.05 !(state.cdata_count));
        (End_CDATA, decr 0.05 !(state.cdata_count));
        (Angle_bracket Opening, 0.1);
        (Angle_bracket Closing, 0.1);
        (Parsing_directive, 0.05);
        (Quote, 0.25);
        (Space, 1.0);
        (* Final state *)
        (Terminate, 0.05);
      ]
      state.state.rng
end

(** {!type:Operation.t} is a fleshed out semanic action. *)
module Operation = struct
  type t =
    | Payload of Payload.t
    | EncodeURI_component
    | EncodeURI
    | Xml_Encode
    | Enclose_tag of Tag.t * Attributes.t
    | Enclose_tag_attribute of (Tag.t  * Attributes.t* Attribute.t  * Attributes.t * Closing_mode.t)
    | Self_closing_tag of (Tag.t * Attributes.t
* Placement.t)
    | Open_tag of (Tag.t  * Attributes.t * Placement.t)
    | Close_tag of (Tag.t  * Attributes.t * Placement.t)
    | Open_XML_comment of Placement.t
    | Close_XML_comment of (Xml_comment_type.t * Placement.t)
    | Enclose_XML_comment of Xml_comment_type.t
    | Begin_CDATA of Placement.t
    | End_CDATA of Placement.t
    | Enclose_CDATA
    | Angle_bracket of (Bracket_type.t * Placement.t)
    | Parsing_directive of Placement.t
    | Enclose_JavaScript_comment
    | Open_JavaScript_comment of Placement.t
    | Close_JavaScript_comment of Placement.t
    | Quote of (Quote.t * Placement.t)
    | Space of Placement.t
  [@@deriving show { with_path = false }, hash, yojson]

  let print acc = function
    | Payload p -> Payload.print p
    | EncodeURI_component -> Util.encode_uri_component acc
    | EncodeURI -> Util.encode_uri acc
    | Xml_Encode -> Util.xml_encode acc
    | Enclose_tag(t,attrs) -> Fmt.str "<%a%a>%s%a" Tag.pf t Attributes.pf attrs acc Tag.pf_closing t
    | Enclose_tag_attribute (t, attrsl, attr, attrsr, Self_closing) ->
      let attrv = Attribute.pr_attr acc attr in
      Fmt.str "<%a%a%s%a/>" Tag.pf t Attributes.pf attrsl attrv Attributes.pf attrsr
    | Enclose_tag_attribute (t, attrsl, attr, attrsr, Unclosed) ->
      let attrv = Attribute.pr_attr acc attr in
      Fmt.str "<%a%a%s%a>" Tag.pf t Attributes.pf attrsl attrv Attributes.pf attrsr
    | Self_closing_tag (t, attrs, Placement.Prepend) -> Fmt.str "<%a%a/>%s" Tag.pf t Attributes.pf attrs acc
    | Self_closing_tag (t, attrs, Placement.Append) -> Fmt.str "%s<%a%a/>" acc Tag.pf t Attributes.pf attrs
    | Open_tag (t, attrs, Placement.Append) -> Fmt.str "%s<%a%a>" acc Tag.pf t Attributes.pf attrs
    | Open_tag (t, attrs, Placement.Prepend) -> Fmt.str "<%a%a>%s" Tag.pf t Attributes.pf attrs acc
    | Close_tag (t, attrs, Placement.Prepend) -> Fmt.str "</%a%a>%s" Tag.pf t Attributes.pf attrs acc
    | Close_tag (t, attrs, Placement.Append) -> Fmt.str "%s</%a%a>" acc Tag.pf t Attributes.pf attrs
    | Open_XML_comment Placement.Prepend -> Fmt.str "%s%s" "<!--" acc
    | Open_XML_comment Placement.Append -> Fmt.str "%s%s" acc "<!--"
    | Close_XML_comment (Bang, Placement.Prepend) -> Fmt.str "%s%s" "--!>" acc
    | Close_XML_comment (Bang, Placement.Append) -> Fmt.str "%s%s" acc "--!>"
    | Close_XML_comment (No_bang, Placement.Prepend) -> Fmt.str "%s%s" "-->" acc
    | Close_XML_comment (No_bang, Placement.Append) -> Fmt.str "%s%s" acc "-->"
    | Enclose_XML_comment No_bang -> Fmt.str "<!--%s-->" acc
    | Enclose_XML_comment Bang -> Fmt.str "<!--%s--!>" acc
    | Begin_CDATA Placement.Prepend -> Fmt.str "%s%s" "<![CDATA[" acc
    | Begin_CDATA Placement.Append -> Fmt.str "%s%s" acc "<![CDATA["
    | End_CDATA Placement.Prepend -> Fmt.str "%s%s" "]]>" acc
    | End_CDATA Placement.Append -> Fmt.str "%s%s" acc "]]>"
    | Enclose_CDATA -> Fmt.str "<![CDATA[%s]]>" acc
    | Parsing_directive Placement.Prepend -> Fmt.str "<!%s" acc
    | Parsing_directive Placement.Append -> Fmt.str "%s<!" acc
    | Angle_bracket (Bracket_type.Opening, Placement.Append) -> Fmt.str "%s<" acc
    | Angle_bracket (Bracket_type.Opening, Placement.Prepend) -> Fmt.str "<%s" acc
    | Angle_bracket (Bracket_type.Closing, Placement.Append) -> Fmt.str "%s>" acc
    | Angle_bracket (Bracket_type.Closing, Placement.Prepend) -> Fmt.str "%s>" acc
    | Enclose_JavaScript_comment -> Fmt.str "/*%s*/" acc
    | Open_JavaScript_comment Placement.Prepend -> Fmt.str "%s%s" "/*" acc
    | Open_JavaScript_comment Placement.Append -> Fmt.str "%s%s" acc "/*"
    | Close_JavaScript_comment Placement.Prepend -> Fmt.str "%s%s" "*/" acc
    | Close_JavaScript_comment Placement.Append -> Fmt.str "%s%s" acc "*/"
    | Quote (q, Placement.Prepend) -> Fmt.str "%a%s" Quote.pf q acc
    | Quote (q, Placement.Append) -> Fmt.str "%s%a" acc Quote.pf q
    | Space Placement.Prepend -> Fmt.str "%s%s" " " acc
    | Space Placement.Append -> Fmt.str "%s%s" acc " "
end

(** A sequence of semantic actions. *)
type t = Operation.t list [@@deriving show { with_path = false }, hash, yojson]

(** Compute the hash of a {!type:t}. *)
let hash_generated gen = [%hash: t] gen

(** [serialize gen] turns a {!type:t} into its JSON representation. *)
let serialize (gen : t) = yojson_of_t gen |> Yojson.Safe.pretty_to_string

(** [deserialize json] turns the JSON representation into the corresponding {!type:t}. *)
let deserialize json = json |> Yojson.Safe.from_string |> t_of_yojson

let max_depth = 25
let min_depth = 7

let rec generate' state acc =
  let module A = Action in
  let module O = Operation in
  if List.length acc >= max_depth then acc
  else
    let a = A.gen state in
    match a with
    | A.Terminate -> acc
    | A.Open_tag ->
        state.open_tag_count := !(state.open_tag_count) + 1;
        let t = Tag.gen state in
        let attrs = Attributes.gen state in
        let p = Placement.gen state in
        generate' state (O.Open_tag (t, attrs, p) :: acc)
    | A.Self_closing_tag ->
        let t = Tag.gen state in
        let attrs = Attributes.gen state in
        let p = Placement.gen state in
        generate' state (O.Self_closing_tag (t, attrs, p) :: acc)
    | A.Enclose_tag ->
        let t = Tag.gen state in
        let attrs = Attributes.gen state in
        generate' state (O.Enclose_tag(t,attrs) :: acc)
    | A.Enclose_tag_attribute cm ->
        let t = Tag.gen state in
        let al = Attributes.gen_up_to 2 state in
        let a = Attribute.gen state in
        let ar = Attributes.gen_up_to 2 state in
        generate' state (O.Enclose_tag_attribute (t, al, a, ar, cm) :: acc)
    | A.Close_tag ->
        let t = Tag.gen state in
        let attrs = Attributes.gen_lp state in
        let p = Placement.gen state in
        generate' state (O.Close_tag (t, attrs, p) :: acc)
    | A.EncodeURI_component ->
        state.uri_encoding_count := !(state.uri_encoding_count) + 1;
        generate' state (O.EncodeURI_component :: acc)
    | A.EncodeURI ->
        state.uri_encoding_count := !(state.uri_encoding_count) + 1;
        generate' state (O.EncodeURI :: acc)
    | A.Xml_Encode ->
        state.xml_encoding_count := !(state.xml_encoding_count) + 1;
        generate' state (O.Xml_Encode :: acc)
    | A.Open_XML_comment ->
        state.xml_comment_count := !(state.xml_comment_count) + 1;
        let p = Placement.gen state in
        generate' state (O.Open_XML_comment p :: acc)
    | A.Close_XML_comment b ->
        let p = Placement.gen state in
        state.xml_comment_count := !(state.xml_comment_count) + 1;
        generate' state (O.Close_XML_comment (b, p) :: acc)
    | A.Enclose_XML_comment b ->
        state.xml_comment_count := !(state.xml_comment_count) + 1;
        generate' state (O.Enclose_XML_comment b :: acc)
    | A.Begin_CDATA ->
        let p = Placement.gen state in
        state.cdata_count := !(state.cdata_count) + 1;
        generate' state (O.Begin_CDATA p :: acc)
    | A.End_CDATA ->
        let p = Placement.gen state in
        state.cdata_count := !(state.cdata_count) + 1;
        generate' state (O.End_CDATA p :: acc)
    | A.Enclose_CDATA ->
        state.cdata_count := !(state.cdata_count) + 1;
        generate' state (O.Enclose_CDATA :: acc)
    | A.Angle_bracket t ->
        let p = Placement.gen state in
        generate' state (O.Angle_bracket (t, p) :: acc)
    | A.Parsing_directive ->
        let p = Placement.gen state in
        generate' state (O.Parsing_directive p :: acc)
    | A.Enclose_JavaScript_comment ->
        state.js_comment_count := !(state.js_comment_count) + 1;
        generate' state (O.Enclose_JavaScript_comment :: acc)
    | A.Open_JavaScript_comment ->
        let p = Placement.gen state in
        state.js_comment_count := !(state.js_comment_count) + 1;
        generate' state (O.Open_JavaScript_comment p :: acc)
    | A.Close_JavaScript_comment ->
        let p = Placement.gen state in
        state.js_comment_count := !(state.js_comment_count) + 1;
        generate' state (O.Close_JavaScript_comment p :: acc)
    | A.Space ->
        let p = Placement.gen state in
        generate' state (O.Space p :: acc)
    | A.Quote ->
        let q = Quote.gen_toplevel state in
        let p = Placement.gen state in
        generate' state (O.Quote (q, p) :: acc)

(** Is the generation result unteresting?

    This allows to reduce the evaluation overhead of payloads that are e.g., only the trigger and closing tags.
*)
let is_unintesting (generated : t) =
  let module O = Operation in
  let interesting =
    generated
    |> List.count (function
         | O.Payload _
         | O.Quote (_, _)
         | O.Close_tag (_,_, _)
         | O.Angle_bracket (_, _)
         | O.Enclose_XML_comment _ | O.Open_XML_comment _ | O.Close_XML_comment _ | O.Enclose_JavaScript_comment
         | O.Open_JavaScript_comment _ | O.Close_JavaScript_comment _ | O.Enclose_CDATA | O.Begin_CDATA _
         | O.End_CDATA _ | O.Xml_Encode | O.EncodeURI | O.EncodeURI_component | O.Space _ ->
             false
         | O.Parsing_directive _ -> false
         | O.Open_tag (_,_, Placement.Append) -> false
         | O.Self_closing_tag _ -> false
         | O.Open_tag _ | O.Enclose_tag _ | O.Enclose_tag_attribute _ -> true)
  in
  let len = generated |> List.length in
  interesting * 3 < len

(** [generate state] generates an (hopefully) interesting {!type:t}. *)
let rec generate st =
  let state = from_state st in
  let payload = Payload.gen state in
  let generated = generate' state [ Operation.Payload payload ] |> List.rev in
  let generated = if List.length generated > min_depth then generated else generate st in
  if is_unintesting generated then generate st else generated
