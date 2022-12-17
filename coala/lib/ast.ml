type bop = Add | Sub | Mul | Div | Modulo | Equal | Neq | Leq | Geq | Less | Gre | And | Or

type typ = Int | String | Char | Float | Bool | Void | Array of typ * int

type expr =
  (* | Int of int  *)
  | Literal of int
  | BoolLit of bool
  | StringLit of string
  | CharLit of string
  | Fliteral of string
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ArrayIntLit of int list
  | ArrayStringLit of string list
  | ArrayBoolLit of bool list
  | ArrayIndexLit of string * expr
  | Noexpr
(* 
  let list_of_string = function
  (* | "[" ^ andar ^ "]" -> split (space "," space) andar  *)
   _ ->  raise (Failure("Not a valid array lit ")) *)
type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | Declare of typ * string
  | DeclareAndAssign of typ * string * expr

type bind = typ * string

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Modulo -> "%"
  | Mul -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Leq -> "<="
  | Geq -> ">="
  | Less -> "<"
  | Gre -> ">"
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Fliteral(l) -> l
  | CharLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrayIntLit(el) -> "[" ^ (String.concat ", " (List.map string_of_int el)) ^ "]"
  | ArrayStringLit(el) -> "[" ^ ((String.concat ", " (el))) ^ "]"
  | ArrayBoolLit(el) -> "[" ^ (String.concat ", " (List.map string_of_bool el))^ "]"
  | ArrayIndexLit(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""

let rec string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Char -> "char"
  | Bool -> "bool"
  | Void -> "void"
  | Array(typ, size) -> "array" ^ "<" ^ (string_of_typ typ) ^ "," ^ string_of_int size ^ ">" 

let typ_of_array = function
    Array(typ, _) -> typ
  | _ -> raise(Failure "Invalid argument")
let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Declare(typ, s) -> string_of_typ typ ^ " " ^ s ^ ";\n"
  | DeclareAndAssign(typ, s, e) -> string_of_typ typ ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"



let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
