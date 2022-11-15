/* Ocamlyacc parser for Coala */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE QUOTES PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT STRING BOOL
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
  decl_rule EOF { $1 }

decl_rule:
  /* nothing */ { ([], [])               }
 | vdecl_rule SEMI decl_rule { (($1 :: fst $3), snd $3) }
 | fdecl_rule decl_rule { (fst $2, ($1 :: snd $2)) }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule SEMI vdecl_list_rule  { $1 :: $3 }

vdecl_rule:
  typ_rule ID { ($1, $2) }


typ_rule:
  INT       { Int  }
  | STRING       { String  }
  | BOOL    { Bool }

/* fdecl_rule */
fdecl_rule:
  vdecl_rule LPAREN formals_opt_rule RPAREN LBRACE vdecl_list_rule stmt_list_rule RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt_rule */
formals_opt_rule:
  /*nothing*/ { [] }
  | formals_list_rule { $1 }

formals_list_rule:
  vdecl_rule { [$1] }
  | vdecl_rule COMMA formals_list_rule { $1::$3 }


stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }
  | RETURN expr_rule SEMI                                      { Return $2       }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | LITERAL                       { Literal $1            }
  | SLIT                          { StringLit $1          }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
  | ID LPAREN args_opt_rule RPAREN     { Call ($1, $3)         }

/* args_opt_rule*/
args_opt_rule:
  /*nothing*/ { [] }
  | args_rule { $1 }

args_rule:
  expr_rule  { [$1] }
  | expr_rule COMMA args_rule { $1::$3 }
