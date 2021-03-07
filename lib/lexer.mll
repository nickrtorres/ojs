{
  open Parser
  exception Lex_error of int * string
}

rule token = parse
  ['\n']                 { Lexing.new_line lexbuf; token lexbuf }
  | [' ']                { token lexbuf }
  (* FIXME floats! *)
  | ['0' - '9'] * as n   { NUM (float_of_string n) }
  | ','                  { COMMA }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '{'                  { LBRACE }
  | '}'                  { RBRACE }
  | '('                  { LPAREN }
  | ')'                  { RPAREN }
  | '<'                  { LT }
  | '>'                  { GT }
  | "<="                 { LTE }
  | ">="                 { GTE }
  | '='                  { EQ }
  | "else"               { ELSE }
  | "false"              { BOOL(false) }
  | "function"           { FUNCTION }
  | "if"                 { IF }
  | "print"              { PRINT }
  | "true"               { BOOL(true) }
  | "var"                { VAR }
  | "while"              { WHILE }
  | ['A' - 'Z' 'a' - 'z'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_'] * as iden  { IDEN(iden) }
  | eof                  { EOF }
  | _ { raise (Lex_error (lexbuf.lex_curr_p.pos_lnum, Lexing.lexeme lexbuf)) }
