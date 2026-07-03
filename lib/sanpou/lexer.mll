{
exception Error of string

let reservedWords = [
  ("def", Parser.DEF);
  ("var", Parser.VAR);
  ("fn", Parser.FN);
  ("mod", Parser.MOD);
  ("fair", Parser.FAIR);
  ("process", Parser.PROCESS);
  ("in", Parser.IN);
  ("while", Parser.WHILE);
  ("if", Parser.IF);
  ("else", Parser.ELSE);
  ("return", Parser.RETURN);
  ("break", Parser.BREAK);
  ("continue", Parser.CONTINUE);
  ("await", Parser.AWAIT);
  ("self", Parser.SELF);
  ("true", Parser.TRUE);
  ("false", Parser.FALSE);
]
}

rule main = parse
| '\n'
    { Lexing.new_line lexbuf; main lexbuf }
| [' ' '\t' '\r']+
    { main lexbuf }
| "//" [^ '\n']* '\n'
    { Lexing.new_line lexbuf; main lexbuf }
| "//" [^ '\n']* eof
    { Parser.EOF }
| ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "=="
    { Parser.EQEQ }
| "!="
    { Parser.NEQ }
| ".."
    { Parser.DOTDOT }
| ":"
    { Parser.COLON }
| "("
    { Parser.LPAREN }
| ")"
    { Parser.RPAREN }
| "["
    { Parser.LBRACKET }
| "]"
    { Parser.RBRACKET }
| "{"
    { Parser.LBRACE }
| "}"
    { Parser.RBRACE }
| ";"
    { Parser.SEMI }
| ","
    { Parser.COMMA }
| "+"
    { Parser.PLUS }
| "-"
    { Parser.MINUS }
| "*"
    { Parser.MULT }
| "<="
    { Parser.LTEQ }
| ">="
    { Parser.GTEQ }
| "&&"
    { Parser.ANDAND }
| "||"
    { Parser.OROR }
| "<"
    { Parser.LT }
| "="
    { Parser.EQ }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    { let id = Lexing.lexeme lexbuf in
      try List.assoc id reservedWords with Not_found -> Parser.ID id }
| eof
    { Parser.EOF }
| _
    { raise (Error ("Unexpected character: '" ^ Lexing.lexeme lexbuf ^ "'")) }
