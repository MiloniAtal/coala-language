type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | QUOTES
  | PLUS
  | MINUS
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
# 4 "lib/coalaparse.mly"
open Ast
# 36 "lib/coalaparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* QUOTES *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* ASSIGN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* LT *);
  269 (* AND *);
  270 (* OR *);
  271 (* IF *);
  272 (* ELSE *);
  273 (* WHILE *);
  274 (* INT *);
  275 (* STRING *);
  276 (* BOOL *);
  277 (* VOID *);
  278 (* RETURN *);
  279 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* LITERAL *);
  281 (* BLIT *);
  282 (* SLIT *);
  283 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\004\000\007\000\007\000\009\000\009\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\012\000\
\012\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\013\000\
\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\003\000\007\000\005\000\003\000\000\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\044\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\004\000\007\000\
\003\000\000\000\000\000\014\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\026\000\028\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\018\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\020\000\000\000\000\000\023\000\000\000\
\000\000\041\000\000\000\030\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\039\000\000\000\022\000\
\043\000\000\000\021\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\026\000\011\000\019\000\037\000\
\020\000\038\000\039\000\046\000\065\000\066\000"

let yysindex = "\005\000\
\171\255\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\020\255\171\255\246\254\000\000\171\255\171\255\000\000\000\000\
\000\000\009\255\045\255\000\000\171\255\034\255\000\000\171\255\
\063\255\035\255\171\255\016\255\035\255\064\255\069\255\016\255\
\000\000\000\000\000\000\131\255\074\255\035\255\111\255\000\000\
\124\255\078\255\016\255\016\255\156\255\068\255\016\255\016\255\
\000\000\000\000\000\000\016\255\016\255\016\255\016\255\016\255\
\016\255\016\255\000\000\000\000\136\255\148\255\000\000\103\255\
\094\255\000\000\156\255\000\000\000\000\145\255\145\255\028\255\
\176\255\169\255\035\255\035\255\016\255\000\000\091\255\000\000\
\000\000\035\255\000\000"

let yyrindex = "\000\000\
\101\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\101\000\000\000\000\000\101\000\099\255\000\000\000\000\
\000\000\117\255\000\000\000\000\000\000\000\000\000\000\029\255\
\000\000\123\255\029\255\000\000\123\255\000\000\000\000\128\255\
\000\000\000\000\000\000\000\255\000\000\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\129\255\000\000\138\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\139\255\
\000\000\000\000\086\255\000\000\000\000\067\255\081\255\062\255\
\085\255\044\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\079\000\000\000\118\000\000\000\000\000\067\000\
\133\000\096\000\228\255\000\000\000\000\088\000"

let yytablesize = 192
let yytable = "\041\000\
\029\000\015\000\029\000\045\000\017\000\001\000\029\000\029\000\
\012\000\029\000\029\000\029\000\029\000\029\000\061\000\062\000\
\016\000\028\000\064\000\067\000\013\000\014\000\029\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\005\000\021\000\
\005\000\005\000\052\000\053\000\028\000\024\000\029\000\033\000\
\034\000\035\000\036\000\005\000\036\000\005\000\036\000\022\000\
\064\000\030\000\005\000\031\000\005\000\005\000\005\000\005\000\
\032\000\036\000\033\000\034\000\035\000\036\000\034\000\027\000\
\034\000\043\000\036\000\032\000\063\000\032\000\044\000\034\000\
\034\000\034\000\034\000\034\000\032\000\032\000\049\000\032\000\
\032\000\033\000\060\000\033\000\034\000\035\000\037\000\035\000\
\037\000\032\000\033\000\033\000\018\000\033\000\033\000\042\000\
\078\000\035\000\035\000\018\000\002\000\013\000\025\000\033\000\
\050\000\025\000\082\000\035\000\037\000\052\000\053\000\051\000\
\054\000\055\000\056\000\057\000\058\000\052\000\053\000\015\000\
\054\000\055\000\056\000\057\000\058\000\077\000\059\000\017\000\
\024\000\025\000\052\000\053\000\047\000\054\000\055\000\056\000\
\057\000\058\000\075\000\048\000\040\000\042\000\052\000\053\000\
\040\000\054\000\055\000\056\000\057\000\058\000\076\000\052\000\
\053\000\023\000\052\000\053\000\056\000\054\000\055\000\056\000\
\057\000\058\000\052\000\053\000\081\000\054\000\055\000\056\000\
\057\000\058\000\079\000\080\000\000\000\000\000\000\000\052\000\
\053\000\083\000\054\000\055\000\056\000\057\000\052\000\053\000\
\000\000\054\000\055\000\056\000\003\000\004\000\005\000\006\000"

let yycheck = "\028\000\
\001\001\010\000\003\001\032\000\013\000\001\000\007\001\008\001\
\000\000\010\001\011\001\012\001\013\001\014\001\043\000\044\000\
\027\001\002\001\047\000\048\000\001\001\002\001\023\001\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\002\001\023\001\
\004\001\005\001\007\001\008\001\002\001\004\001\004\001\024\001\
\025\001\026\001\027\001\015\001\001\001\017\001\003\001\003\001\
\077\000\015\001\022\001\017\001\024\001\025\001\026\001\027\001\
\022\001\014\001\024\001\025\001\026\001\027\001\001\001\001\001\
\003\001\002\001\023\001\001\001\001\001\003\001\002\001\010\001\
\011\001\012\001\013\001\014\001\010\001\011\001\005\001\013\001\
\014\001\001\001\005\001\003\001\023\001\001\001\001\001\003\001\
\003\001\023\001\010\001\011\001\014\000\013\001\014\001\029\000\
\003\001\013\001\014\001\021\000\000\000\003\001\024\000\023\001\
\038\000\027\000\016\001\023\001\023\001\007\001\008\001\001\001\
\010\001\011\001\012\001\013\001\014\001\007\001\008\001\003\001\
\010\001\011\001\012\001\013\001\014\001\023\001\003\001\005\001\
\001\001\001\001\007\001\008\001\002\001\010\001\011\001\012\001\
\013\001\014\001\003\001\009\001\003\001\003\001\007\001\008\001\
\027\000\010\001\011\001\012\001\013\001\014\001\003\001\007\001\
\008\001\021\000\007\001\008\001\012\001\010\001\011\001\012\001\
\013\001\014\001\007\001\008\001\077\000\010\001\011\001\012\001\
\013\001\014\001\075\000\076\000\255\255\255\255\255\255\007\001\
\008\001\082\000\010\001\011\001\012\001\013\001\007\001\008\001\
\255\255\010\001\011\001\012\001\018\001\019\001\020\001\021\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  QUOTES\000\
  PLUS\000\
  MINUS\000\
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
# 30 "lib/coalaparse.mly"
                ( _1 )
# 228 "lib/coalaparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "lib/coalaparse.mly"
                ( ([], [])               )
# 234 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 34 "lib/coalaparse.mly"
                             ( ((_1 :: fst _3), snd _3) )
# 242 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 35 "lib/coalaparse.mly"
                        ( (fst _2, (_1 :: snd _2)) )
# 250 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "lib/coalaparse.mly"
                                ( []       )
# 256 "lib/coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 39 "lib/coalaparse.mly"
                                     ( _1 :: _3 )
# 264 "lib/coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "lib/coalaparse.mly"
              ( (_1, _2) )
# 272 "lib/coalaparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "lib/coalaparse.mly"
            ( Int  )
# 278 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "lib/coalaparse.mly"
            ( String  )
# 284 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "lib/coalaparse.mly"
            ( Bool )
# 290 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "lib/coalaparse.mly"
            ( Void)
# 296 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 54 "lib/coalaparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 314 "lib/coalaparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "lib/coalaparse.mly"
              ( [] )
# 320 "lib/coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 67 "lib/coalaparse.mly"
                      ( _1 )
# 327 "lib/coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 70 "lib/coalaparse.mly"
             ( [_1] )
# 334 "lib/coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 71 "lib/coalaparse.mly"
                                       ( _1::_3 )
# 342 "lib/coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "lib/coalaparse.mly"
                                ( []     )
# 348 "lib/coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 76 "lib/coalaparse.mly"
                                ( _1::_2 )
# 356 "lib/coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 79 "lib/coalaparse.mly"
                                                          ( Expr _1         )
# 363 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 80 "lib/coalaparse.mly"
                                                          ( Block _2        )
# 370 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 81 "lib/coalaparse.mly"
                                                          ( If (_3, _5, _7) )
# 379 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 82 "lib/coalaparse.mly"
                                                          ( While (_3,_5)   )
# 387 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt_rule) in
    Obj.repr(
# 83 "lib/coalaparse.mly"
                                                          ( Return _2       )
# 394 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "lib/coalaparse.mly"
                  ( Noexpr )
# 400 "lib/coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 87 "lib/coalaparse.mly"
                       ( _1 )
# 407 "lib/coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "lib/coalaparse.mly"
                                  ( BoolLit _1            )
# 414 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "lib/coalaparse.mly"
                                  ( Literal _1            )
# 421 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "lib/coalaparse.mly"
                                  ( StringLit _1          )
# 428 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "lib/coalaparse.mly"
                                  ( Id _1                 )
# 435 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 94 "lib/coalaparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 443 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "lib/coalaparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 451 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "lib/coalaparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 459 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "lib/coalaparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 467 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "lib/coalaparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 475 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "lib/coalaparse.mly"
                                  ( Binop (_1, And, _3)   )
# 483 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "lib/coalaparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 491 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "lib/coalaparse.mly"
                                  ( Assign (_1, _3)       )
# 499 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 102 "lib/coalaparse.mly"
                                  ( _2                    )
# 506 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt_rule) in
    Obj.repr(
# 103 "lib/coalaparse.mly"
                                       ( Call (_1, _3)         )
# 514 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "lib/coalaparse.mly"
              ( [] )
# 520 "lib/coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 108 "lib/coalaparse.mly"
              ( _1 )
# 527 "lib/coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 111 "lib/coalaparse.mly"
             ( [_1] )
# 534 "lib/coalaparse.ml"
               : 'args_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 112 "lib/coalaparse.mly"
                              ( _1::_3 )
# 542 "lib/coalaparse.ml"
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
