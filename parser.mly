%{
    open Session
%}

%token OPENP CLOSP
%token LT GT
%token WHYNOT COLON COMMA SEMICOLON ARROW
%token ZERO OMEGA END CLOSE
%token INJ1 INJ2 STRING INT
%token PLUS AND
%token <string> ID
%token EOF

%left PLUS
%left AND
%right SEMICOLON
%right COMMA

%start s
%type <Session.session> s

%%

s:
    | ZERO { [] }
    | OMEGA { [[]] }
    | END { [[Nil End]] }
    | CLOSE { [[Nil Close]] }
    | WHYNOT ID COLON LT s GT { [[Nil (Client($2,$5))]] }
    | s PLUS s { cup $1 $3 }
    | s AND s { cupcup $1 $3 }
    | c SEMICOLON s { cons $1 $3 }
    | OPENP s CLOSP { $2 }

c:
    | ID ARROW id_list COLON m { Msg($1, $3, $5) }

m :
    | INJ1 { Inj1 }
    | INJ2 { Inj2 }
    | INT { Int }
    | STRING { String }
    | LT s GT { Sub $2 }

id_list :
    | ID COMMA id_list { $1 :: $3 }
    | ID { [$1] }
