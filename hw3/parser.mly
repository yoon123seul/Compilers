/* TODO: Complete the grammar specification for the S language. */

%{
    open S
%}

%token <int> NUM
%token <string> ID
%token INT PLUS MINUS STAR SLASH EQUAL EQUALEQUAL LE LT GE GT NOT AND OR IF ELSE WHILE DO READ PRINT SEMICOLON
%token LBRACE RBRACE LBLOCK RBLOCK LPAREN RPAREN EOF


%start program
%type <S.program> program
%left PLUS MINUS
%left STAR SLASH
%left AND 
%left OR
%left NOT
%left EQUALEQUAL LE LT GE GT
%left LPAREN RPAREN
%left LBRACE RBRACE
%left LBLOCK RBLOCK
%left EQUAL
%left SEMICOLON



%%

program:
    block EOF { $1 }

block:
    LBRACE decls stmts RBRACE { ($2, $3) }

decls:
    | decl decls { $1 :: $2 }
    |  { [] }  // 여기서 입실론 처리 어떻게 하는 지?

decl:
    typ ID SEMICOLON { ($1, $2) }

typ:
    | INT { S.TINT }
    | INT LBLOCK NUM RBLOCK { S.TARR $3 }

stmts:
    | stmt stmts { $1 :: $2 }
    | { [] }

stmt:
    | lv EQUAL e SEMICOLON { S.ASSIGN ($1, $3) }
    | lv PLUS PLUS SEMICOLON { S.ASSIGN ($1, (S.ADD (S.LV $1, S.NUM 1))) } // 여기서 1을 어떻게 더하는지
    | IF LPAREN e RPAREN stmt ELSE stmt { S.IF ($3, $5, $7) }
    | WHILE LPAREN e RPAREN stmt { S.WHILE ($3, $5) }
    | DO stmt WHILE LPAREN e RPAREN SEMICOLON { S.DOWHILE ($2, $5) }
    | READ LPAREN ID RPAREN SEMICOLON { S.READ $3 }
    | PRINT LPAREN e RPAREN SEMICOLON { S.PRINT $3 }
    | block { S.BLOCK ($1) }

lv:
    | ID { S.ID $1 }
    | ID LBLOCK e RBLOCK { S.ARR ($1, $3) } 

e:
    | NUM { S.NUM $1 }
    | lv { LV $1 }
    | e PLUS e { ADD ($1, $3) }
    | e MINUS e { SUB ($1, $3) }
    | e STAR e { MUL ($1, $3) }
    | e SLASH e { DIV ($1, $3) }
    | MINUS e { MINUS $2 }
    | NOT e { NOT $2 }
    | e LT e { LT ($1, $3) }
    | e LE e { LE ($1, $3) }
    | e GT e { GT ($1, $3) }
    | e GE e { GE ($1, $3) }
    | e EQUALEQUAL e { EQ ($1, $3) }
    | e AND e { AND ($1, $3) }
    | e OR e { OR ($1, $3) }
    | LPAREN e RPAREN { $2 }