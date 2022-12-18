type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | QUOTES
  | PLUS
  | MINUS
  | MULT
  | DIV
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | INT
  | STRING
  | BOOL
  | VOID
  | RETURN
  | COMMA
  | LITERAL of (int)
  | BLIT of (bool)
  | SLIT of (string)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "coalaparse.mly"
open Ast
# 38 "coalaparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* QUOTES *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* MULT *);
  266 (* DIV *);
  267 (* ASSIGN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* AND *);
  272 (* OR *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* INT *);
  277 (* STRING *);
  278 (* BOOL *);
  279 (* VOID *);
  280 (* RETURN *);
  281 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  282 (* LITERAL *);
  283 (* BLIT *);
  284 (* SLIT *);
  285 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\004\000\007\000\007\000\009\000\009\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\007\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\003\000\007\000\005\000\003\000\003\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\048\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\004\000\007\000\
\003\000\000\000\000\000\014\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\028\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\018\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\042\000\020\000\000\000\000\000\023\000\000\000\
\000\000\045\000\000\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\000\000\000\000\000\022\000\047\000\025\000\000\000\
\021\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\000\000\034\000\019\000\035\000\
\020\000\036\000\037\000\043\000\065\000\066\000"

let yysindex = "\017\000\
\029\255\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\007\255\029\255\249\254\000\000\029\255\029\255\000\000\000\000\
\000\000\012\255\021\255\000\000\029\255\063\255\000\000\019\255\
\032\255\019\255\064\255\070\255\032\255\000\000\000\000\000\000\
\255\254\053\255\078\255\019\255\177\255\201\255\092\255\032\255\
\032\255\239\255\098\255\032\255\032\255\001\255\000\000\000\000\
\000\000\032\255\032\255\032\255\032\255\032\255\032\255\032\255\
\032\255\032\255\000\000\000\000\215\255\229\255\000\000\158\255\
\100\255\000\000\239\255\000\000\032\255\044\255\044\255\239\255\
\239\255\010\000\010\000\172\255\002\000\249\255\019\255\019\255\
\032\255\000\000\187\255\086\255\000\000\000\000\000\000\019\255\
\000\000"

let yyrindex = "\000\000\
\111\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\111\000\000\000\000\000\111\000\110\255\000\000\000\000\
\000\000\111\255\000\000\000\000\000\000\000\000\000\000\115\255\
\000\000\115\255\000\000\000\000\118\255\000\000\000\000\000\000\
\061\255\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\000\000\127\255\000\000\113\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\129\255\
\000\000\000\000\010\255\000\000\000\000\077\255\093\255\054\255\
\062\255\114\255\130\255\109\255\134\255\135\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\085\000\067\000\000\000\000\000\143\000\000\000\237\255\
\119\000\182\255\231\255\000\000\000\000\060\000"

let yytablesize = 280
let yytable = "\038\000\
\044\000\068\000\012\000\042\000\084\000\085\000\039\000\013\000\
\014\000\045\000\041\000\069\000\041\000\089\000\061\000\062\000\
\048\000\001\000\064\000\067\000\025\000\016\000\026\000\022\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\025\000\041\000\027\000\021\000\028\000\003\000\004\000\
\005\000\006\000\029\000\083\000\030\000\031\000\032\000\033\000\
\003\000\004\000\005\000\006\000\052\000\053\000\034\000\064\000\
\034\000\030\000\031\000\032\000\033\000\031\000\035\000\031\000\
\035\000\040\000\024\000\031\000\031\000\031\000\031\000\041\000\
\031\000\031\000\031\000\031\000\031\000\032\000\034\000\032\000\
\018\000\046\000\047\000\032\000\032\000\031\000\035\000\018\000\
\032\000\032\000\032\000\032\000\032\000\033\000\015\000\033\000\
\060\000\017\000\063\000\033\000\033\000\032\000\082\000\088\000\
\033\000\033\000\033\000\033\000\033\000\038\000\002\000\038\000\
\013\000\015\000\036\000\044\000\036\000\033\000\026\000\017\000\
\038\000\038\000\038\000\038\000\038\000\036\000\036\000\027\000\
\036\000\036\000\037\000\046\000\037\000\038\000\039\000\040\000\
\039\000\040\000\036\000\023\000\086\000\037\000\037\000\011\000\
\037\000\037\000\000\000\000\000\039\000\039\000\040\000\000\000\
\011\000\000\000\037\000\011\000\011\000\000\000\039\000\040\000\
\000\000\000\000\000\000\011\000\050\000\051\000\052\000\053\000\
\000\000\054\000\055\000\056\000\057\000\058\000\000\000\000\000\
\000\000\049\000\050\000\051\000\052\000\053\000\081\000\050\000\
\051\000\052\000\053\000\087\000\054\000\055\000\056\000\057\000\
\058\000\050\000\051\000\052\000\053\000\000\000\054\000\055\000\
\056\000\057\000\058\000\059\000\000\000\000\000\000\000\050\000\
\051\000\052\000\053\000\000\000\054\000\055\000\056\000\057\000\
\058\000\079\000\000\000\000\000\000\000\050\000\051\000\052\000\
\053\000\000\000\054\000\055\000\056\000\057\000\058\000\080\000\
\000\000\000\000\000\000\050\000\051\000\052\000\053\000\000\000\
\054\000\055\000\056\000\057\000\058\000\050\000\051\000\052\000\
\053\000\000\000\054\000\055\000\056\000\057\000\058\000\050\000\
\051\000\052\000\053\000\000\000\054\000\055\000\056\000\057\000\
\050\000\051\000\052\000\053\000\000\000\054\000\055\000\056\000\
\050\000\051\000\052\000\053\000\000\000\000\000\000\000\056\000"

let yycheck = "\025\000\
\002\001\001\001\000\000\029\000\079\000\080\000\026\000\001\001\
\002\001\011\001\001\001\011\001\003\001\088\000\040\000\041\000\
\036\000\001\000\044\000\045\000\002\001\029\001\004\001\003\001\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\002\001\025\001\017\001\025\001\019\001\020\001\021\001\
\022\001\023\001\024\001\069\000\026\001\027\001\028\001\029\001\
\020\001\021\001\022\001\023\001\009\001\010\001\001\001\081\000\
\003\001\026\001\027\001\028\001\029\001\001\001\001\001\003\001\
\003\001\002\001\004\001\007\001\008\001\009\001\010\001\002\001\
\012\001\013\001\014\001\015\001\016\001\001\001\025\001\003\001\
\014\000\029\001\005\001\007\001\008\001\025\001\025\001\021\000\
\012\001\013\001\014\001\015\001\016\001\001\001\010\000\003\001\
\005\001\013\000\001\001\007\001\008\001\025\001\003\001\018\001\
\012\001\013\001\014\001\015\001\016\001\001\001\000\000\003\001\
\003\001\003\001\001\001\003\001\003\001\025\001\001\001\005\001\
\012\001\013\001\014\001\015\001\016\001\012\001\013\001\001\001\
\015\001\016\001\001\001\003\001\003\001\025\001\001\001\001\001\
\003\001\003\001\025\001\021\000\081\000\012\001\013\001\001\000\
\015\001\016\001\255\255\255\255\015\001\016\001\016\001\255\255\
\010\000\255\255\025\001\013\000\014\000\255\255\025\001\025\001\
\255\255\255\255\255\255\021\000\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\255\255\001\001\007\001\008\001\009\001\010\001\025\001\007\001\
\008\001\009\001\010\001\001\001\012\001\013\001\014\001\015\001\
\016\001\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\003\001\255\255\255\255\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\014\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  QUOTES\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  STRING\000\
  BOOL\000\
  VOID\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  SLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl_rule) in
    Obj.repr(
# 31 "coalaparse.mly"
                ( _1 )
# 259 "coalaparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "coalaparse.mly"
                ( ([], [])               )
# 265 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 35 "coalaparse.mly"
                             ( ((_1 :: fst _3), snd _3) )
# 273 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 36 "coalaparse.mly"
                        ( (fst _2, (_1 :: snd _2)) )
# 281 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "coalaparse.mly"
                                ( []       )
# 287 "coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 40 "coalaparse.mly"
                                     ( _1 :: _3 )
# 295 "coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "coalaparse.mly"
              ( (_1, _2) )
# 303 "coalaparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "coalaparse.mly"
            ( Int  )
# 309 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "coalaparse.mly"
            ( String  )
# 315 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "coalaparse.mly"
            ( Bool )
# 321 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "coalaparse.mly"
            ( Void)
# 327 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 55 "coalaparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      body=_6
    }
  )
# 343 "coalaparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "coalaparse.mly"
              ( [] )
# 349 "coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 67 "coalaparse.mly"
                      ( _1 )
# 356 "coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 70 "coalaparse.mly"
             ( [_1] )
# 363 "coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 71 "coalaparse.mly"
                                       ( _1::_3 )
# 371 "coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "coalaparse.mly"
                                ( []     )
# 377 "coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 76 "coalaparse.mly"
                                ( _1::_2 )
# 385 "coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 79 "coalaparse.mly"
                                                          ( Expr _1         )
# 392 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 80 "coalaparse.mly"
                                                          ( Block _2        )
# 399 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 81 "coalaparse.mly"
                                                          ( If (_3, _5, _7) )
# 408 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 82 "coalaparse.mly"
                                                          ( While (_3,_5)   )
# 416 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt_rule) in
    Obj.repr(
# 83 "coalaparse.mly"
                                                          ( Return _2       )
# 423 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "coalaparse.mly"
                                                          ( Declare (_1, _2)      )
# 431 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 85 "coalaparse.mly"
                                                          ( DeclareAndAssign (_1, _2, _4)      )
# 440 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "coalaparse.mly"
                  ( Noexpr )
# 446 "coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 89 "coalaparse.mly"
                       ( _1 )
# 453 "coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 93 "coalaparse.mly"
                                  ( BoolLit _1            )
# 460 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "coalaparse.mly"
                                  ( Literal _1            )
# 467 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "coalaparse.mly"
                                  ( StringLit _1          )
# 474 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "coalaparse.mly"
                                  ( Id _1                 )
# 481 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "coalaparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 489 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "coalaparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 497 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "coalaparse.mly"
                                  ( Binop (_1, Mul, _3)   )
# 505 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "coalaparse.mly"
                                  ( Binop (_1, Div, _3)   )
# 513 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "coalaparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 521 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "coalaparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 529 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 104 "coalaparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 537 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 105 "coalaparse.mly"
                                  ( Binop (_1, And, _3)   )
# 545 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 106 "coalaparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 553 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 107 "coalaparse.mly"
                                  ( Assign (_1, _3)       )
# 561 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 108 "coalaparse.mly"
                                  ( _2                    )
# 568 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt_rule) in
    Obj.repr(
# 109 "coalaparse.mly"
                                       ( Call (_1, _3)         )
# 576 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "coalaparse.mly"
              ( [] )
# 582 "coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 114 "coalaparse.mly"
              ( _1 )
# 589 "coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 117 "coalaparse.mly"
             ( [_1] )
# 596 "coalaparse.ml"
               : 'args_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 118 "coalaparse.mly"
                              ( _1::_3 )
# 604 "coalaparse.ml"
               : 'args_rule))
(* Entry program_rule *)
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
let program_rule (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
