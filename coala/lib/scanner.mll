(* Ocamllex scanner for Coala *)

{ open Coalaparse }

let digit = ['0'-'9']
let negatives = '-'['0'-'9']
let digits = digit+
let letter = ['a'-'z' 'A'-'Z']
let quotes = ['"']
let squotes = [''']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULT }
| '/'      { DIV }
| '%'      { MODULO }
| '='      { ASSIGN }
| ">="      { GEQ }
| "<="      { LEQ }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"    { FLOAT }
| "char"   { CHAR }
| "string" { STRING }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| ('-'digit+ | digit+)  as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| ('-'digits | digits) '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| quotes _* quotes as lem { SLIT(lem) }
| squotes _* squotes as lem { CLIT(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
