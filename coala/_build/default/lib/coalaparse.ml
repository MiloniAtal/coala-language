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
# 35 "lib/coalaparse.ml"
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
  277 (* RETURN *);
  278 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  279 (* LITERAL *);
  280 (* BLIT *);
  281 (* SLIT *);
  282 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\004\000\007\000\007\000\009\000\009\000\008\000\
\008\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\013\000\013\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\008\000\000\000\001\000\001\000\003\000\000\000\
\002\000\002\000\003\000\007\000\005\000\003\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\041\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\004\000\007\000\003\000\
\000\000\000\000\013\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\023\000\025\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\017\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\019\000\000\000\000\000\022\000\000\000\000\000\038\000\
\000\000\027\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\000\000\021\000\040\000\000\000\
\020\000"

let yydgoto = "\002\000\
\006\000\007\000\008\000\009\000\025\000\010\000\018\000\036\000\
\019\000\037\000\038\000\063\000\064\000"

let yysindex = "\008\000\
\052\255\000\000\000\000\000\000\000\000\000\000\017\000\005\255\
\052\255\012\255\000\000\052\255\052\255\000\000\000\000\000\000\
\019\255\044\255\000\000\052\255\046\255\000\000\052\255\055\255\
\040\255\052\255\011\255\040\255\056\255\057\255\011\255\000\000\
\000\000\000\000\073\255\063\255\040\255\132\255\000\000\152\255\
\079\255\011\255\011\255\140\255\011\255\011\255\000\000\000\000\
\000\000\011\255\011\255\011\255\011\255\011\255\011\255\011\255\
\000\000\000\000\164\255\176\255\000\000\116\255\059\255\000\000\
\184\255\000\000\000\000\149\255\149\255\013\255\192\255\124\255\
\040\255\040\255\011\255\000\000\069\255\000\000\000\000\040\255\
\000\000"

let yyrindex = "\000\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\060\000\000\000\000\000\060\000\083\255\000\000\000\000\000\000\
\092\255\000\000\000\000\000\000\000\000\000\000\028\255\000\000\
\093\255\028\255\000\000\093\255\000\000\000\000\000\000\000\000\
\000\000\000\000\066\255\000\000\093\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\104\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\106\255\000\000\000\000\
\009\255\000\000\000\000\086\255\100\255\080\255\103\255\000\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\252\255\092\000\000\000\093\000\000\000\000\000\230\255\
\100\000\222\255\229\255\000\000\046\000"

let yytablesize = 204
let yytable = "\040\000\
\033\000\041\000\033\000\044\000\014\000\012\000\013\000\016\000\
\001\000\034\000\048\000\034\000\027\000\033\000\059\000\060\000\
\011\000\062\000\065\000\050\000\051\000\033\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\005\000\034\000\005\000\
\005\000\032\000\033\000\034\000\035\000\015\000\077\000\078\000\
\020\000\027\000\005\000\028\000\005\000\081\000\021\000\062\000\
\005\000\023\000\005\000\005\000\005\000\005\000\029\000\026\000\
\030\000\042\000\043\000\002\000\031\000\076\000\032\000\033\000\
\034\000\035\000\026\000\047\000\026\000\003\000\004\000\005\000\
\026\000\026\000\045\000\026\000\026\000\026\000\026\000\026\000\
\031\000\046\000\031\000\058\000\080\000\012\000\029\000\026\000\
\029\000\031\000\031\000\031\000\031\000\031\000\014\000\029\000\
\029\000\016\000\029\000\029\000\030\000\031\000\030\000\032\000\
\017\000\032\000\037\000\029\000\039\000\030\000\030\000\017\000\
\030\000\030\000\024\000\032\000\032\000\024\000\039\000\022\000\
\079\000\030\000\050\000\051\000\032\000\052\000\053\000\054\000\
\055\000\056\000\050\000\051\000\049\000\052\000\053\000\054\000\
\055\000\075\000\050\000\051\000\061\000\052\000\053\000\054\000\
\055\000\056\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000\050\000\051\000\000\000\050\000\051\000\
\054\000\052\000\053\000\054\000\055\000\056\000\073\000\000\000\
\000\000\000\000\050\000\051\000\000\000\052\000\053\000\054\000\
\055\000\056\000\074\000\000\000\000\000\000\000\050\000\051\000\
\000\000\052\000\053\000\054\000\055\000\056\000\050\000\051\000\
\000\000\052\000\053\000\054\000\055\000\056\000\050\000\051\000\
\000\000\052\000\053\000\054\000"

let yycheck = "\027\000\
\001\001\028\000\003\001\031\000\009\000\001\001\002\001\012\000\
\001\000\001\001\037\000\003\001\002\001\014\001\042\000\043\000\
\000\000\045\000\046\000\007\001\008\001\022\001\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\002\001\022\001\004\001\
\005\001\023\001\024\001\025\001\026\001\026\001\073\000\074\000\
\022\001\002\001\015\001\004\001\017\001\080\000\003\001\075\000\
\021\001\004\001\023\001\024\001\025\001\026\001\015\001\001\001\
\017\001\002\001\002\001\000\000\021\001\003\001\023\001\024\001\
\025\001\026\001\001\001\005\001\003\001\018\001\019\001\020\001\
\007\001\008\001\002\001\010\001\011\001\012\001\013\001\014\001\
\001\001\009\001\003\001\005\001\016\001\003\001\001\001\022\001\
\003\001\010\001\011\001\012\001\013\001\014\001\003\001\010\001\
\011\001\005\001\013\001\014\001\001\001\022\001\003\001\001\001\
\013\000\003\001\003\001\022\001\003\001\010\001\011\001\020\000\
\013\001\014\001\023\000\013\001\014\001\026\000\026\000\020\000\
\075\000\022\001\007\001\008\001\022\001\010\001\011\001\012\001\
\013\001\014\001\007\001\008\001\001\001\010\001\011\001\012\001\
\013\001\022\001\007\001\008\001\001\001\010\001\011\001\012\001\
\013\001\014\001\007\001\008\001\255\255\010\001\011\001\012\001\
\013\001\014\001\003\001\007\001\008\001\255\255\007\001\008\001\
\012\001\010\001\011\001\012\001\013\001\014\001\003\001\255\255\
\255\255\255\255\007\001\008\001\255\255\010\001\011\001\012\001\
\013\001\014\001\003\001\255\255\255\255\255\255\007\001\008\001\
\255\255\010\001\011\001\012\001\013\001\014\001\007\001\008\001\
\255\255\010\001\011\001\012\001\013\001\014\001\007\001\008\001\
\255\255\010\001\011\001\012\001"

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
# 229 "lib/coalaparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "lib/coalaparse.mly"
                ( ([], [])               )
# 235 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 34 "lib/coalaparse.mly"
                             ( ((_1 :: fst _3), snd _3) )
# 243 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 35 "lib/coalaparse.mly"
                        ( (fst _2, (_1 :: snd _2)) )
# 251 "lib/coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "lib/coalaparse.mly"
                                ( []       )
# 257 "lib/coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 39 "lib/coalaparse.mly"
                                     ( _1 :: _3 )
# 265 "lib/coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "lib/coalaparse.mly"
              ( (_1, _2) )
# 273 "lib/coalaparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "lib/coalaparse.mly"
            ( Int  )
# 279 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "lib/coalaparse.mly"
                 ( String  )
# 285 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "lib/coalaparse.mly"
            ( Bool )
# 291 "lib/coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 53 "lib/coalaparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 309 "lib/coalaparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "lib/coalaparse.mly"
              ( [] )
# 315 "lib/coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 66 "lib/coalaparse.mly"
                      ( _1 )
# 322 "lib/coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 69 "lib/coalaparse.mly"
             ( [_1] )
# 329 "lib/coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 70 "lib/coalaparse.mly"
                                       ( _1::_3 )
# 337 "lib/coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "lib/coalaparse.mly"
                                ( []     )
# 343 "lib/coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 75 "lib/coalaparse.mly"
                                ( _1::_2 )
# 351 "lib/coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 78 "lib/coalaparse.mly"
                                                          ( Expr _1         )
# 358 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 79 "lib/coalaparse.mly"
                                                          ( Block _2        )
# 365 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 80 "lib/coalaparse.mly"
                                                          ( If (_3, _5, _7) )
# 374 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 81 "lib/coalaparse.mly"
                                                          ( While (_3,_5)   )
# 382 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 82 "lib/coalaparse.mly"
                                                               ( Return _2       )
# 389 "lib/coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 85 "lib/coalaparse.mly"
                                  ( BoolLit _1            )
# 396 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "lib/coalaparse.mly"
                                  ( Literal _1            )
# 403 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "lib/coalaparse.mly"
                                  ( StringLit _1          )
# 410 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "lib/coalaparse.mly"
                                  ( Id _1                 )
# 417 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 89 "lib/coalaparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 425 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 90 "lib/coalaparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 433 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 91 "lib/coalaparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 441 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 92 "lib/coalaparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 449 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 93 "lib/coalaparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 457 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 94 "lib/coalaparse.mly"
                                  ( Binop (_1, And, _3)   )
# 465 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "lib/coalaparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 473 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "lib/coalaparse.mly"
                                  ( Assign (_1, _3)       )
# 481 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 97 "lib/coalaparse.mly"
                                  ( _2                    )
# 488 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt_rule) in
    Obj.repr(
# 98 "lib/coalaparse.mly"
                                       ( Call (_1, _3)         )
# 496 "lib/coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "lib/coalaparse.mly"
              ( [] )
# 502 "lib/coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 103 "lib/coalaparse.mly"
              ( _1 )
# 509 "lib/coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 106 "lib/coalaparse.mly"
             ( [_1] )
# 516 "lib/coalaparse.ml"
               : 'args_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 107 "lib/coalaparse.mly"
                              ( _1::_3 )
# 524 "lib/coalaparse.ml"
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
