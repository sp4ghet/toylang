{
  open Parser

let reservedWords = [
  ("else", ELSE);
  ("if", IF);
  ("then", THEN);
  ("true", TRUE);
  ("false", FALSE);
  ("let", LET);
  ("in", IN);
]

exception LexError of string
exception End_Of_File

let error lexbuf msg =
  let pos = Lexing.lexeme_start lexbuf in
  raise (LexError ("lexing error: " ^ (string_of_int pos) ^ msg))

let next_line = Lexing.new_line
}

rule main = parse
  (*ignore whitespace *)
  | '\n' {next_line lexbuf; main lexbuf}
  | [' ' '\009' '\012']+     { main lexbuf }
  | "(*" { comment 0 lexbuf }
  | "-"? ['0'-'9']+
      { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";;" { SEMISEMI }
  | "+" { PLUS }
  | "*" { MULT }
  | "-" { MINUS }
  | "<" { LT }
  | ">" { GT }
  | "&&" { LAND }
  | "||" { LOR }
  | "=" { EQ }
  | ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
      { let id = Lexing.lexeme lexbuf in
        try
          List.assoc id reservedWords
        with
        _ -> ID id
      }
  | eof { raise End_Of_File }
  | _ { error lexbuf "Invalid token" }
and comment n = parse
  "*)" { if n = 0 then main lexbuf else comment (n-1) lexbuf }
  | "(*" { comment (n+1) lexbuf }
  | eof { error lexbuf "end of input in comment, missing: *)?" }
  | _ { comment n lexbuf }
