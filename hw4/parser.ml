type token =
  | NUM of (int)
  | ID of (string)
  | INT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | EQUALEQUAL
  | LE
  | LT
  | GE
  | GT
  | NOT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | DO
  | READ
  | PRINT
  | SEMICOLON
  | LBRACE
  | RBRACE
  | LBLOCK
  | RBLOCK
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
    open S
# 38 "parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* STAR *);
  263 (* SLASH *);
  264 (* EQUAL *);
  265 (* EQUALEQUAL *);
  266 (* LE *);
  267 (* LT *);
  268 (* GE *);
  269 (* GT *);
  270 (* NOT *);
  271 (* AND *);
  272 (* OR *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* DO *);
  277 (* READ *);
  278 (* PRINT *);
  279 (* SEMICOLON *);
  280 (* LBRACE *);
  281 (* RBRACE *);
  282 (* LBLOCK *);
  283 (* RBLOCK *);
  284 (* LPAREN *);
  285 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\006\000\006\000\004\000\
\004\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\008\000\008\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\004\000\002\000\000\000\003\000\001\000\004\000\002\000\
\000\000\004\000\004\000\007\000\005\000\007\000\005\000\005\000\
\001\000\001\000\004\000\001\000\001\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\008\000\
\000\000\000\000\005\000\007\000\020\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\011\000\010\000\035\000\
\000\000\000\000\000\000\000\000\032\000\029\000\028\000\031\000\
\030\000\000\000\000\000\000\000\013\000\000\000\015\000\016\000\
\000\000\000\000\012\000\014\000"

let yydgoto = "\002\000\
\004\000\018\000\007\000\019\000\008\000\009\000\020\000\041\000\
\042\000"

let yysindex = "\006\000\
\243\254\000\000\009\255\000\000\017\000\248\254\052\255\009\255\
\018\255\000\000\024\255\016\255\000\255\021\255\052\255\022\255\
\027\255\000\000\035\255\052\255\001\255\000\000\038\255\036\255\
\039\255\039\255\039\255\043\255\063\255\039\255\000\000\000\000\
\062\255\039\255\000\000\000\000\000\000\039\255\039\255\039\255\
\000\000\252\255\105\255\120\255\049\255\064\255\135\255\073\255\
\009\000\235\255\034\000\150\255\039\255\039\255\039\255\039\255\
\039\255\039\255\039\255\039\255\039\255\039\255\039\255\000\000\
\052\255\052\255\039\255\075\255\076\255\000\000\000\000\000\000\
\235\255\235\255\018\000\018\000\000\000\000\000\000\000\000\000\
\000\000\026\000\034\000\082\255\000\000\165\255\000\000\000\000\
\052\255\081\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\002\255\000\000\000\000\103\255\088\255\002\255\
\000\000\000\000\000\000\079\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\088\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\197\255\041\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\200\255\226\255\074\255\211\255\000\000\000\000\000\000\000\000\
\000\000\185\255\180\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\106\000\111\000\102\000\000\000\000\000\242\255\249\255\
\232\255"

let yytablesize = 303
let yytable = "\021\000\
\028\000\043\000\044\000\004\000\033\000\047\000\001\000\021\000\
\034\000\049\000\003\000\006\000\021\000\050\000\051\000\052\000\
\010\000\011\000\004\000\023\000\004\000\004\000\004\000\004\000\
\024\000\004\000\004\000\026\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\037\000\
\012\000\025\000\086\000\038\000\027\000\027\000\027\000\027\000\
\027\000\029\000\084\000\085\000\039\000\012\000\030\000\027\000\
\027\000\021\000\021\000\031\000\035\000\045\000\036\000\027\000\
\046\000\048\000\040\000\027\000\013\000\027\000\014\000\015\000\
\016\000\017\000\091\000\003\000\067\000\024\000\024\000\024\000\
\024\000\021\000\018\000\018\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\068\000\018\000\018\000\070\000\
\024\000\087\000\088\000\089\000\024\000\018\000\024\000\092\000\
\006\000\018\000\005\000\018\000\053\000\054\000\055\000\056\000\
\009\000\057\000\058\000\059\000\060\000\061\000\022\000\062\000\
\063\000\032\000\000\000\053\000\054\000\055\000\056\000\000\000\
\057\000\058\000\059\000\060\000\061\000\065\000\062\000\063\000\
\000\000\000\000\053\000\054\000\055\000\056\000\000\000\057\000\
\058\000\059\000\060\000\061\000\066\000\062\000\063\000\000\000\
\000\000\053\000\054\000\055\000\056\000\000\000\057\000\058\000\
\059\000\060\000\061\000\069\000\062\000\063\000\000\000\000\000\
\053\000\054\000\055\000\056\000\000\000\057\000\058\000\059\000\
\060\000\061\000\072\000\062\000\063\000\000\000\000\000\034\000\
\034\000\034\000\034\000\000\000\033\000\033\000\033\000\033\000\
\000\000\090\000\034\000\034\000\000\000\000\000\000\000\033\000\
\026\000\026\000\034\000\022\000\022\000\000\000\034\000\033\000\
\034\000\000\000\000\000\033\000\000\000\033\000\025\000\025\000\
\025\000\025\000\000\000\026\000\000\000\000\000\022\000\026\000\
\000\000\026\000\022\000\000\000\022\000\023\000\023\000\000\000\
\000\000\025\000\000\000\000\000\000\000\025\000\000\000\025\000\
\055\000\056\000\000\000\057\000\058\000\059\000\060\000\061\000\
\023\000\062\000\063\000\000\000\023\000\000\000\023\000\053\000\
\054\000\055\000\056\000\000\000\057\000\058\000\059\000\060\000\
\061\000\000\000\062\000\063\000\053\000\054\000\055\000\056\000\
\000\000\057\000\058\000\059\000\060\000\061\000\064\000\062\000\
\063\000\000\000\057\000\058\000\059\000\060\000\061\000\071\000\
\062\000\063\000\057\000\058\000\059\000\060\000\061\000\000\000\
\000\000\063\000\057\000\058\000\059\000\060\000\061\000"

let yycheck = "\007\000\
\015\000\026\000\027\000\002\001\004\001\030\000\001\000\015\000\
\008\001\034\000\024\001\003\001\020\000\038\000\039\000\040\000\
\000\000\026\001\017\001\002\001\019\001\020\001\021\001\022\001\
\001\001\024\001\025\001\028\001\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\001\001\
\002\001\026\001\067\000\005\001\004\001\005\001\006\001\007\001\
\028\001\028\001\065\000\066\000\014\001\002\001\028\001\015\001\
\016\001\065\000\066\000\025\001\023\001\019\001\027\001\023\001\
\002\001\004\001\028\001\027\001\017\001\029\001\019\001\020\001\
\021\001\022\001\089\000\024\001\028\001\004\001\005\001\006\001\
\007\001\089\000\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\029\001\015\001\016\001\023\001\
\023\001\023\001\023\001\018\001\027\001\023\001\029\001\023\001\
\002\001\027\001\001\000\029\001\004\001\005\001\006\001\007\001\
\025\001\009\001\010\001\011\001\012\001\013\001\008\000\015\001\
\016\001\020\000\255\255\004\001\005\001\006\001\007\001\255\255\
\009\001\010\001\011\001\012\001\013\001\029\001\015\001\016\001\
\255\255\255\255\004\001\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\012\001\013\001\029\001\015\001\016\001\255\255\
\255\255\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\029\001\015\001\016\001\255\255\255\255\
\004\001\005\001\006\001\007\001\255\255\009\001\010\001\011\001\
\012\001\013\001\029\001\015\001\016\001\255\255\255\255\004\001\
\005\001\006\001\007\001\255\255\004\001\005\001\006\001\007\001\
\255\255\029\001\015\001\016\001\255\255\255\255\255\255\015\001\
\004\001\005\001\023\001\004\001\005\001\255\255\027\001\023\001\
\029\001\255\255\255\255\027\001\255\255\029\001\004\001\005\001\
\006\001\007\001\255\255\023\001\255\255\255\255\023\001\027\001\
\255\255\029\001\027\001\255\255\029\001\004\001\005\001\255\255\
\255\255\023\001\255\255\255\255\255\255\027\001\255\255\029\001\
\006\001\007\001\255\255\009\001\010\001\011\001\012\001\013\001\
\023\001\015\001\016\001\255\255\027\001\255\255\029\001\004\001\
\005\001\006\001\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\004\001\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\012\001\013\001\027\001\015\001\
\016\001\255\255\009\001\010\001\011\001\012\001\013\001\023\001\
\015\001\016\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\016\001\009\001\010\001\011\001\012\001\013\001"

let yynames_const = "\
  INT\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  EQUAL\000\
  EQUALEQUAL\000\
  LE\000\
  LT\000\
  GE\000\
  GT\000\
  NOT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  READ\000\
  PRINT\000\
  SEMICOLON\000\
  LBRACE\000\
  RBRACE\000\
  LBLOCK\000\
  RBLOCK\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 33 "parser.mly"
              ( _1 )
# 263 "parser.ml"
               : S.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 36 "parser.mly"
                              ( (_2, _3) )
# 271 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 39 "parser.mly"
                 ( _1 :: _2 )
# 279 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
       ( [] )
# 285 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                     ( (_1, _2) )
# 293 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
          ( S.TINT )
# 299 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 47 "parser.mly"
                            ( S.TARR _3 )
# 306 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 50 "parser.mly"
                 ( _1 :: _2 )
# 314 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
      ( [] )
# 320 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lv) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 54 "parser.mly"
                           ( S.ASSIGN (_1, _3) )
# 328 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lv) in
    Obj.repr(
# 55 "parser.mly"
                             ( S.ASSIGN (_1, (S.ADD (S.LV _1, S.NUM 1))) )
# 335 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'e) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                                        ( S.IF (_3, _5, _7) )
# 344 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "parser.mly"
                                 ( S.WHILE (_3, _5) )
# 352 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    Obj.repr(
# 58 "parser.mly"
                                              ( S.DOWHILE (_2, _5) )
# 360 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 59 "parser.mly"
                                      ( S.READ _3 )
# 367 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    Obj.repr(
# 60 "parser.mly"
                                      ( S.PRINT _3 )
# 374 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 61 "parser.mly"
            ( S.BLOCK (_1) )
# 381 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
         ( S.ID _1 )
# 388 "parser.ml"
               : 'lv))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 65 "parser.mly"
                         ( S.ARR (_1, _3) )
# 396 "parser.ml"
               : 'lv))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
          ( S.NUM _1 )
# 403 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lv) in
    Obj.repr(
# 69 "parser.mly"
         ( LV _1 )
# 410 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 70 "parser.mly"
               ( ADD (_1, _3) )
# 418 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 71 "parser.mly"
                ( SUB (_1, _3) )
# 426 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 72 "parser.mly"
               ( MUL (_1, _3) )
# 434 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 73 "parser.mly"
                ( DIV (_1, _3) )
# 442 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 74 "parser.mly"
              ( MINUS _2 )
# 449 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 75 "parser.mly"
            ( NOT _2 )
# 456 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 76 "parser.mly"
             ( LT (_1, _3) )
# 464 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 77 "parser.mly"
             ( LE (_1, _3) )
# 472 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 78 "parser.mly"
             ( GT (_1, _3) )
# 480 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 79 "parser.mly"
             ( GE (_1, _3) )
# 488 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 80 "parser.mly"
                     ( EQ (_1, _3) )
# 496 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 81 "parser.mly"
              ( AND (_1, _3) )
# 504 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 82 "parser.mly"
             ( OR (_1, _3) )
# 512 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 83 "parser.mly"
                      ( _2 )
# 519 "parser.ml"
               : 'e))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : S.program)
