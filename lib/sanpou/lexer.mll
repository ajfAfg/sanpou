{
let trivia_buf = Buffer.create 128

let flush_trivia () =
  let s = Buffer.contents trivia_buf in
  Buffer.clear trivia_buf;
  s

let reservedWords = [
  ("def", fun t -> Parser.DEF t);
  ("let", fun t -> Parser.LET t);
  ("fn", fun t -> Parser.FN t);
  ("mod", fun t -> Parser.MOD t);
  ("process", fun t -> Parser.PROCESS t);
  ("in", fun t -> Parser.IN t);
  ("while", fun t -> Parser.WHILE t);
  ("if", fun t -> Parser.IF t);
  ("return", fun t -> Parser.RETURN t);
  ("break", fun t -> Parser.BREAK t);
  ("await", fun t -> Parser.AWAIT t);
  ("true", fun t -> Parser.TRUE t);
  ("false", fun t -> Parser.FALSE t);
]
}

rule main = parse
| '\n'
    { Buffer.add_char trivia_buf '\n'; Lexing.new_line lexbuf; main lexbuf }
| [' ' '\t' '\r']+
    { Buffer.add_string trivia_buf (Lexing.lexeme lexbuf); main lexbuf }
| "//" [^ '\n']* '\n'
    { Buffer.add_string trivia_buf (Lexing.lexeme lexbuf); Lexing.new_line lexbuf; main lexbuf }
| "//" [^ '\n']* eof
    { Buffer.add_string trivia_buf (Lexing.lexeme lexbuf); Parser.EOF (flush_trivia ()) }
| ['0'-'9']+
    { let t = flush_trivia () in
      Parser.INTV (t, int_of_string (Lexing.lexeme lexbuf)) }
| "=="
    { Parser.EQEQ (flush_trivia ()) }
| ".."
    { Parser.DOTDOT (flush_trivia ()) }
| "("
    { Parser.LPAREN (flush_trivia ()) }
| ")"
    { Parser.RPAREN (flush_trivia ()) }
| "{"
    { Parser.LBRACE (flush_trivia ()) }
| "}"
    { Parser.RBRACE (flush_trivia ()) }
| ";"
    { Parser.SEMI (flush_trivia ()) }
| ","
    { Parser.COMMA (flush_trivia ()) }
| "+"
    { Parser.PLUS (flush_trivia ()) }
| "-"
    { Parser.MINUS (flush_trivia ()) }
| "*"
    { Parser.MULT (flush_trivia ()) }
| "<="
    { Parser.LTEQ (flush_trivia ()) }
| ">="
    { Parser.GTEQ (flush_trivia ()) }
| "&&"
    { Parser.ANDAND (flush_trivia ()) }
| "<"
    { Parser.LT (flush_trivia ()) }
| "="
    { Parser.EQ (flush_trivia ()) }
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    { let t = flush_trivia () in
      let id = Lexing.lexeme lexbuf in
      try
        (List.assoc id reservedWords) t
      with
        Not_found -> Parser.ID (t, id)
    }
| eof
    { Parser.EOF (flush_trivia ()) }
