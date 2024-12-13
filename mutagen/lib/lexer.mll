{
  open Lexing
  open Containers
  module P = Parser

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }

  exception SyntaxError of string
}

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)
let upper_alpha = ['A'-'Z']
let lower_alpha = ['a'-'z']
let alpha = lower_alpha | upper_alpha
let tag_name = lower_alpha | upper_alpha | '-'
let newline = ('\n' | "\r\n")

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| [' ' '\t']
    { token lexbuf }
| "#tag" { P.TAG }
| "#attr" { P.ATTRIBUTE }
| "#comment" { P.COMMENT }
| "#cdata" { P.CDATA }
| "#text" { P.TEXT_NODE }
| "#document-fragment" { P.DOCUMENT_FRAGMENT }
| "#document_type" { P.DOCUMENT_TYPE }
| "#document" { P.DOCUMENT }
| "," { P.COMMA }
| "(" { P.L_PAREN }
| ")" { P.R_PAREN }
| "[" { P.L_ANGLE_PAREN }
| "]" { P.R_ANGLE_PAREN }
| tag_name+ as n
  { P.TEXT (n) }
| '"' { P.STRING (ruleInQuotes (Buffer.create 32) lexbuf) }
| newline | eof {
    P.EOF
  }
| _ as c
    { raise @@ SyntaxError (Fmt.str "At offset %d: unexpected character '%s'" (Lexing.lexeme_start lexbuf) (Char.to_string c)) }
and ruleInQuotes acc = parse
  | eof         { raise @@ SyntaxError "EOF before terminating quote" }
  | newline         {
      next_line lexbuf;
      raise @@ SyntaxError "EOL before terminating quote"
    }
  | "\\\""      { 
      Buffer.add_string acc "\"";
      ruleInQuotes acc lexbuf
    }
  | [^'"' '\\' '\n']* as c { let s = String.escaped c in
                       (*Printf.printf "next char '%s'\n%!" s;*)
                       Buffer.add_string acc s;
                       ruleInQuotes acc lexbuf }
  | '"'         { Buffer.contents acc }
  | _           { raise @@ SyntaxError "ruleInQuotes" }


