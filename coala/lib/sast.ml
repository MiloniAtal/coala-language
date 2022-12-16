(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SStringLit of string
  | SCharLit of string
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SArrayIntLit of int list
  | SArrayStringLit of string list 
  | SArrayBoolLit of bool list
  (* call *)
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SDeclare of typ * string
  | SDeclareAndAssign of typ * string * sexpr

(* func_def: ret_typ fname formals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SFliteral(l) -> l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SStringLit(l) -> l
      | SCharLit(l) -> l
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SArrayStringLit (el)-> "string " ^  "[" ^ ((String.concat ", " (el))) ^ "]"
      | SArrayIntLit (el) -> "int " ^ "[" ^ (String.concat ", " (List.map string_of_int el)) ^ "]"
      | SArrayBoolLit (el) -> "bool " ^ "[" ^ (String.concat ", " (List.map string_of_bool el))^ "]"
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SNoexpr -> ""
      ) ^ ")"


let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SDeclare(ty, var) -> string_of_typ ty ^ " " ^ var ^ ";\n"
  | SDeclareAndAssign(ty, var, e) -> string_of_typ ty ^ " " ^ var ^ " = " ^ string_of_sexpr e ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
