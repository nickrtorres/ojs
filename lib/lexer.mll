{
  open Parser
  exception Lex_error of int * string
}

rule token = parse
  ['\n']                 { Lexing.new_line lexbuf; token lexbuf }
  | [' ']                { token lexbuf }
  (* FIXME floats! *)
  | ['0' - '9'] * as n   { NUM (float_of_string n) }
  | "var"                { VAR }
  | "true"               { BOOL(true) }
  | "false"              { BOOL(false) }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '('                  { LPAREN }
  | ')'                  { RPAREN }
  | '<'                  { LT }
  | '>'                  { GT }
  | "<="                 { LTE }
  | ">="                 { GTE }
  | "while"              { WHILE }
  | "print"              { PRINT }
  | '='                  { EQ }
  | ['A' - 'Z' 'a' - 'z'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_'] * as iden  { IDEN(iden) }
  | eof                  { EOF }
  | _ { raise (Lex_error (lexbuf.lex_curr_p.pos_lnum, Lexing.lexeme lexbuf)) }
