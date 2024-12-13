%token L_ANGLE_PAREN R_ANGLE_PAREN
%token L_PAREN R_PAREN
%token TAG ATTRIBUTE COMMENT TEXT_NODE DOCUMENT_FRAGMENT DOCUMENT CDATA DOCUMENT_TYPE
%token COMMA
%token EOF
%token <string> TEXT
%token <string> STRING

%start <Ast.t option> main

%%

main:
  | a = tag EOF { Some(a) }
  | EOF { None }

tag:
  | L_PAREN DOCUMENT_FRAGMENT  L_ANGLE_PAREN  children = separated_list(COMMA, tag) R_ANGLE_PAREN R_PAREN {  Ast.Document_fragment(children) }
  | L_PAREN TAG n = TEXT L_ANGLE_PAREN  attrs = separated_list(COMMA, attribute) R_ANGLE_PAREN L_ANGLE_PAREN  children = separated_list(COMMA, tag) R_ANGLE_PAREN R_PAREN { let name = CCString.lowercase_ascii n in Ast.Tag((String.lowercase_ascii name), attrs, None, children) }
  | L_PAREN TAG n = STRING L_ANGLE_PAREN  attrs = separated_list(COMMA, attribute) R_ANGLE_PAREN L_ANGLE_PAREN  children = separated_list(COMMA, tag) R_ANGLE_PAREN R_PAREN { let name = CCString.lowercase_ascii n in Ast.Tag((String.lowercase_ascii name), attrs, None, children) }
  //| L_PAREN TAG DOCUMENT L_ANGLE_PAREN  attrs = separated_list(COMMA, attribute) R_ANGLE_PAREN L_ANGLE_PAREN  children = separated_list(COMMA, tag) R_ANGLE_PAREN R_PAREN { let name = "document" in Ast.Tag((String.lowercase_ascii name), attrs, None, children) }
  | L_PAREN DOCUMENT  L_ANGLE_PAREN  children = separated_list(COMMA, tag) R_ANGLE_PAREN R_PAREN { Ast.Document(children) }
  | L_PAREN COMMENT c = STRING R_PAREN { Ast.Comment(c) }
  | L_PAREN DOCUMENT_TYPE t = TEXT R_PAREN { Ast.Document_type(t) }
  | L_PAREN CDATA c = STRING R_PAREN { Ast.Cdata(c) }
  | L_PAREN TEXT_NODE c = STRING R_PAREN { Ast.Text(c) }
  ;

attribute:
  L_PAREN ATTRIBUTE name = STRING value = STRING R_PAREN { Ast.({key = name; value = value }) }
  ;
