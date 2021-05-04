{open Parser}

rule read = parse
  | [' ' '\t'] { read lexbuf }
  | [ '\n' ] { Lexing.new_line lexbuf; read lexbuf }
  | '(' { OPENP }
  | ')' { CLOSP }
  | '<' { LT }
  | '>' { GT }
  | '0' { ZERO }
  | '?' { WHYNOT }
  | '+' { PLUS }
  | '&' { AND }
  | ':' { COLON }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | "%inj1" { INJ1 }
  | "%inj2" { INJ2 }
  | "string" { STRING }
  | "int" { INT }
  | "omega" { OMEGA }
  | "->" { ARROW }
  | "end" { END }
  | "close" { CLOSE }
  | ['A' - 'Z' 'a' - 'z']['A' - 'Z' 'a' - 'z' '0' - '9' '_' '\'']* as x { ID x }
  | eof { EOF }
