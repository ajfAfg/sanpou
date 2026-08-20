{
exception Error of string

let unexpected lexbuf =
  raise
    (Error
       (Printf.sprintf "unexpected character '%s' at offset %d"
          (Lexing.lexeme lexbuf)
          (Lexing.lexeme_start lexbuf)))
}

rule token = parse
| [' ' '\t' '\r' '\n']+
    { token lexbuf }
| '-'? ['0'-'9']+
    { Json_parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
| "true"
    { Json_parser.TRUE }
| "false"
    { Json_parser.FALSE }
| '"'
    { Json_parser.STRING (string_body (Buffer.create 16) lexbuf) }
| '{'
    { Json_parser.LBRACE }
| '}'
    { Json_parser.RBRACE }
| '['
    { Json_parser.LBRACKET }
| ']'
    { Json_parser.RBRACKET }
| ':'
    { Json_parser.COLON }
| ','
    { Json_parser.COMMA }
| eof
    { Json_parser.EOF }
| _
    { unexpected lexbuf }

(* The writer escapes only the double quote, backslash, LF, TAB, and CR; any
   other backslash sequence is not an escape and both characters stand for
   themselves, matching what the writer emits for a string containing a lone
   backslash followed by such a character. *)
and string_body buf = parse
| '"'
    { Buffer.contents buf }
| "\\\""
    { Buffer.add_char buf '"'; string_body buf lexbuf }
| "\\\\"
    { Buffer.add_char buf '\\'; string_body buf lexbuf }
| "\\n"
    { Buffer.add_char buf '\n'; string_body buf lexbuf }
| "\\t"
    { Buffer.add_char buf '\t'; string_body buf lexbuf }
| "\\r"
    { Buffer.add_char buf '\r'; string_body buf lexbuf }
| '\\' (_ as c)
    { Buffer.add_char buf '\\'; Buffer.add_char buf c; string_body buf lexbuf }
| eof
    { raise (Error "unterminated string literal") }
| _ as c
    { Buffer.add_char buf c; string_body buf lexbuf }
