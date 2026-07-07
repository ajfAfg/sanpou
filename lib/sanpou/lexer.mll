{
exception Error of string

let reservedWords = [
  ("def", Parser.DEF);
  ("atom", Parser.ATOM);
  ("var", Parser.VAR);
  ("procedure", Parser.PROCEDURE_KW);
  ("property", Parser.PROPERTY);
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
  ("assert", Parser.ASSERT);
  ("forall", Parser.FORALL);
  ("exists", Parser.EXISTS);
  ("either", Parser.EITHER);
  ("or", Parser.OR);
  ("with", Parser.WITH);
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
(* Minimal string literals: any run of characters other than a double quote
   or newline, with no escape sequences. *)
| '"' ([^ '"' '\n']* as s) '"'
    { Parser.STRINGV s }
| '"' [^ '"' '\n']* ('\n' | eof)
    { raise (Error "Unterminated string literal (strings cannot span lines)") }
| "=="
    { Parser.EQEQ }
| "!="
    { Parser.NEQ }
| ".."
    { Parser.DOTDOT }
| "."
    { Parser.DOT }
| "->"
    { Parser.ARROW }
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
| ":"
    { Parser.COLON }
| ","
    { Parser.COMMA }
| "+"
    { Parser.PLUS }
| "-"
    { Parser.MINUS }
| "*"
    { Parser.MULT }
| "/"
    { Parser.DIV }
| "%"
    { Parser.PERCENT }
| "!"
    { Parser.NOT }
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
| ">"
    { Parser.GT }
| "="
    { Parser.EQ }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    { let id = Lexing.lexeme lexbuf in
      try List.assoc id reservedWords with Not_found -> Parser.ID id }
| eof
    { Parser.EOF }
| _
    { raise (Error ("Unexpected character: '" ^ Lexing.lexeme lexbuf ^ "'")) }
