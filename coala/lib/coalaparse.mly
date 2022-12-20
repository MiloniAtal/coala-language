/* Ocamlyacc parser for Coala */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE SQLBRACE SQRBRACE QUOTES PLUS MINUS MODULO MULT DIV ASSIGN 
%token EQ NEQ LEQ GEQ LT GT AND OR CONCAT
%token IF ELSE WHILE FOR INT STRING CHAR BOOL FLOAT VOID ARRAY
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> SLIT ALIT FLIT CLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left CONCAT
%left OR
%left AND
%left EQ NEQ
%left LEQ GEQ
%left LT GT
%left PLUS MINUS MODULO
%left MULT DIV

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
  | STRING  { String  }
  | CHAR    { Char }
  | FLOAT { Float }
  | BOOL    { Bool }
  | VOID    { Void}
  | ARRAY  LT typ_rule COMMA LITERAL GT { Array($3, $5)}

/* fdecl_rule */
fdecl_rule:
  vdecl_rule LPAREN formals_opt_rule RPAREN LBRACE stmt_list_rule RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      body=$6
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
  | FOR LPAREN expr_opt_rule SEMI expr_rule SEMI expr_opt_rule RPAREN stmt_rule { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }
  | RETURN expr_opt_rule SEMI                             { Return $2       }
  | typ_rule ID SEMI                                      { Declare ($1, $2)      }
  | typ_rule ID ASSIGN expr_rule SEMI                     { DeclareAndAssign ($1, $2, $4)      }

expr_opt_rule:
    /* nothing */ { Noexpr }
  | expr_rule          { $1 }

array_int_list_rule:
  /* nothing */               { []     }
  | LITERAL            { [$1] }
  | LITERAL COMMA array_int_list_rule  { $1::$3 }

array_string_list_rule:
  /* nothing */               { []     }
  | SLIT            { [$1] }
  | SLIT COMMA array_string_list_rule  { $1::$3 }

array_bool_list_rule:
  /* nothing */               { []     }
  | BLIT            { [$1] }
  | BLIT COMMA array_bool_list_rule  { $1::$3 }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | LITERAL                       { Literal $1            }
  | FLIT	                        { Fliteral($1)           }
  | SLIT                          { StringLit $1          }
  | CLIT                          { CharLit $1            }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule MULT expr_rule      { Binop ($1, Mul, $3)   }
  | expr_rule DIV expr_rule       { Binop ($1, Div, $3)   }
  | expr_rule MODULO expr_rule     { Binop ($1, Modulo, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LEQ expr_rule       { Binop ($1, Leq, $3)   }
  | expr_rule GEQ expr_rule       { Binop ($1, Geq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule GT expr_rule        { Binop ($1, Gre, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | SLIT CONCAT SLIT              { Concat ($1, $3)       }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
  | ID LPAREN args_opt_rule RPAREN     { Call ($1, $3)         }
  | SQLBRACE array_int_list_rule SQRBRACE { ArrayIntLit $2 }
  | SQLBRACE array_bool_list_rule SQRBRACE { ArrayBoolLit $2 }
  | SQLBRACE array_string_list_rule SQRBRACE { ArrayStringLit $2 }
  | ID SQLBRACE expr_rule SQRBRACE {ArrayIndexLit ($1, $3)}

/* args_opt_rule*/
args_opt_rule:
  /*nothing*/ { [] }
  | args_rule { $1 }

args_rule:
  expr_rule  { [$1] }
  | expr_rule COMMA args_rule { $1::$3 }
