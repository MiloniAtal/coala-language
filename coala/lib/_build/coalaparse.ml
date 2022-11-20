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
# 4 "coalaparse.mly"
open Ast
# 36 "coalaparse.ml"
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
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\007\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\003\000\007\000\005\000\003\000\003\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\046\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\004\000\007\000\
\003\000\000\000\000\000\014\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\028\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\018\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\020\000\000\000\000\000\023\000\000\000\000\000\043\000\
\000\000\024\000\000\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\000\000\000\000\
\022\000\045\000\025\000\000\000\021\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\000\000\034\000\019\000\035\000\
\020\000\036\000\037\000\043\000\063\000\064\000"

let yysindex = "\013\000\
\029\255\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\060\255\029\255\036\255\000\000\029\255\029\255\000\000\000\000\
\000\000\012\255\073\255\000\000\029\255\088\255\000\000\019\255\
\030\255\019\255\096\255\107\255\030\255\000\000\000\000\000\000\
\008\255\085\255\111\255\019\255\117\255\137\255\116\255\030\255\
\030\255\177\255\122\255\030\255\030\255\087\255\000\000\000\000\
\000\000\030\255\030\255\030\255\030\255\030\255\030\255\030\255\
\000\000\000\000\149\255\161\255\000\000\251\254\131\255\000\000\
\177\255\000\000\030\255\000\000\000\000\098\255\098\255\112\255\
\192\255\185\255\019\255\019\255\030\255\000\000\125\255\126\255\
\000\000\000\000\000\000\019\255\000\000"

let yyrindex = "\000\000\
\141\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\141\000\000\000\000\000\141\000\140\255\000\000\000\000\
\000\000\143\255\000\000\000\000\000\000\000\000\000\000\148\255\
\000\000\148\255\000\000\000\000\153\255\000\000\000\000\000\000\
\057\255\000\000\000\000\148\255\000\000\000\000\000\000\000\000\
\000\000\154\255\000\000\155\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\162\255\000\000\000\000\
\050\255\000\000\000\000\000\000\000\000\076\255\090\255\071\255\
\094\255\010\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\065\000\045\000\000\000\000\000\101\000\000\000\242\255\
\145\000\102\000\231\255\000\000\000\000\090\000"

let yytablesize = 204
let yytable = "\038\000\
\012\000\050\000\051\000\042\000\052\000\053\000\054\000\055\000\
\056\000\044\000\038\000\039\000\038\000\001\000\059\000\060\000\
\045\000\077\000\062\000\065\000\025\000\048\000\026\000\038\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\025\000\
\038\000\027\000\021\000\028\000\003\000\004\000\005\000\006\000\
\029\000\079\000\030\000\031\000\032\000\033\000\003\000\004\000\
\005\000\006\000\039\000\062\000\039\000\030\000\031\000\032\000\
\033\000\031\000\018\000\031\000\013\000\014\000\016\000\031\000\
\031\000\018\000\031\000\031\000\031\000\031\000\031\000\036\000\
\039\000\036\000\015\000\022\000\034\000\017\000\034\000\031\000\
\036\000\036\000\036\000\036\000\036\000\034\000\034\000\066\000\
\034\000\034\000\035\000\024\000\035\000\036\000\037\000\067\000\
\037\000\040\000\034\000\035\000\035\000\011\000\035\000\035\000\
\050\000\051\000\037\000\037\000\041\000\054\000\011\000\046\000\
\035\000\011\000\011\000\047\000\037\000\049\000\050\000\051\000\
\058\000\011\000\061\000\050\000\051\000\083\000\052\000\053\000\
\054\000\055\000\056\000\050\000\051\000\078\000\052\000\053\000\
\054\000\055\000\056\000\057\000\002\000\084\000\013\000\050\000\
\051\000\015\000\052\000\053\000\054\000\055\000\056\000\075\000\
\017\000\026\000\027\000\050\000\051\000\042\000\052\000\053\000\
\054\000\055\000\056\000\076\000\044\000\023\000\082\000\050\000\
\051\000\000\000\052\000\053\000\054\000\055\000\056\000\000\000\
\080\000\081\000\000\000\000\000\000\000\000\000\000\000\050\000\
\051\000\085\000\052\000\053\000\054\000\055\000\056\000\050\000\
\051\000\000\000\052\000\053\000\054\000\055\000\050\000\051\000\
\000\000\052\000\053\000\054\000"

let yycheck = "\025\000\
\000\000\007\001\008\001\029\000\010\001\011\001\012\001\013\001\
\014\001\002\001\001\001\026\000\003\001\001\000\040\000\041\000\
\009\001\023\001\044\000\045\000\002\001\036\000\004\001\014\001\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\002\001\
\023\001\015\001\023\001\017\001\018\001\019\001\020\001\021\001\
\022\001\067\000\024\001\025\001\026\001\027\001\018\001\019\001\
\020\001\021\001\001\001\077\000\003\001\024\001\025\001\026\001\
\027\001\001\001\014\000\003\001\001\001\002\001\027\001\007\001\
\008\001\021\000\010\001\011\001\012\001\013\001\014\001\001\001\
\023\001\003\001\010\000\003\001\001\001\013\000\003\001\023\001\
\010\001\011\001\012\001\013\001\014\001\010\001\011\001\001\001\
\013\001\014\001\001\001\004\001\003\001\023\001\001\001\009\001\
\003\001\002\001\023\001\010\001\011\001\001\000\013\001\014\001\
\007\001\008\001\013\001\014\001\002\001\012\001\010\000\027\001\
\023\001\013\000\014\000\005\001\023\001\001\001\007\001\008\001\
\005\001\021\000\001\001\007\001\008\001\001\001\010\001\011\001\
\012\001\013\001\014\001\007\001\008\001\003\001\010\001\011\001\
\012\001\013\001\014\001\003\001\000\000\016\001\003\001\007\001\
\008\001\003\001\010\001\011\001\012\001\013\001\014\001\003\001\
\005\001\001\001\001\001\007\001\008\001\003\001\010\001\011\001\
\012\001\013\001\014\001\003\001\003\001\021\000\077\000\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\014\001\255\255\
\075\000\076\000\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\084\000\010\001\011\001\012\001\013\001\014\001\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\007\001\008\001\
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
# 30 "coalaparse.mly"
                ( _1 )
# 232 "coalaparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "coalaparse.mly"
                ( ([], [])               )
# 238 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 34 "coalaparse.mly"
                             ( ((_1 :: fst _3), snd _3) )
# 246 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_rule) in
    Obj.repr(
# 35 "coalaparse.mly"
                        ( (fst _2, (_1 :: snd _2)) )
# 254 "coalaparse.ml"
               : 'decl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "coalaparse.mly"
                                ( []       )
# 260 "coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 39 "coalaparse.mly"
                                     ( _1 :: _3 )
# 268 "coalaparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "coalaparse.mly"
              ( (_1, _2) )
# 276 "coalaparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "coalaparse.mly"
            ( Int  )
# 282 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "coalaparse.mly"
            ( String  )
# 288 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "coalaparse.mly"
            ( Bool )
# 294 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "coalaparse.mly"
            ( Void)
# 300 "coalaparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 54 "coalaparse.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      body=_6
    }
  )
# 316 "coalaparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "coalaparse.mly"
              ( [] )
# 322 "coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 66 "coalaparse.mly"
                      ( _1 )
# 329 "coalaparse.ml"
               : 'formals_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 69 "coalaparse.mly"
             ( [_1] )
# 336 "coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list_rule) in
    Obj.repr(
# 70 "coalaparse.mly"
                                       ( _1::_3 )
# 344 "coalaparse.ml"
               : 'formals_list_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "coalaparse.mly"
                                ( []     )
# 350 "coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 75 "coalaparse.mly"
                                ( _1::_2 )
# 358 "coalaparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 78 "coalaparse.mly"
                                                          ( Expr _1         )
# 365 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 79 "coalaparse.mly"
                                                          ( Block _2        )
# 372 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 80 "coalaparse.mly"
                                                          ( If (_3, _5, _7) )
# 381 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 81 "coalaparse.mly"
                                                          ( While (_3,_5)   )
# 389 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt_rule) in
    Obj.repr(
# 82 "coalaparse.mly"
                                                          ( Return _2       )
# 396 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 83 "coalaparse.mly"
                                                          ( Declare (_1, _2)      )
# 404 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 84 "coalaparse.mly"
                                                          ( DeclareAndAssign (_1, _2, _4)      )
# 413 "coalaparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "coalaparse.mly"
                  ( Noexpr )
# 419 "coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 88 "coalaparse.mly"
                       ( _1 )
# 426 "coalaparse.ml"
               : 'expr_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 91 "coalaparse.mly"
                                  ( BoolLit _1            )
# 433 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "coalaparse.mly"
                                  ( Literal _1            )
# 440 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "coalaparse.mly"
                                  ( StringLit _1          )
# 447 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "coalaparse.mly"
                                  ( Id _1                 )
# 454 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "coalaparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 462 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "coalaparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 470 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "coalaparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 478 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "coalaparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 486 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "coalaparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 494 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "coalaparse.mly"
                                  ( Binop (_1, And, _3)   )
# 502 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "coalaparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 510 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "coalaparse.mly"
                                  ( Assign (_1, _3)       )
# 518 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 103 "coalaparse.mly"
                                  ( _2                    )
# 525 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt_rule) in
    Obj.repr(
# 104 "coalaparse.mly"
                                       ( Call (_1, _3)         )
# 533 "coalaparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "coalaparse.mly"
              ( [] )
# 539 "coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 109 "coalaparse.mly"
              ( _1 )
# 546 "coalaparse.ml"
               : 'args_opt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 112 "coalaparse.mly"
             ( [_1] )
# 553 "coalaparse.ml"
               : 'args_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args_rule) in
    Obj.repr(
# 113 "coalaparse.mly"
                              ( _1::_3 )
# 561 "coalaparse.ml"
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
