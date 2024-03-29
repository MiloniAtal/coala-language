(* Semantic checking for the Coala compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Verify a declaration has no void type*)
  let check_declare (ty : typ) (s : string) =
  match ty with
	| Void -> raise (Failure ("illegal void " ^ (string_of_typ ty) ^ " " ^ s))
  | _ -> ()
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      rtyp = Void;
      fname = name; 
      formals = [(ty, "x")]; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("prints", String); ("printb", Bool); ("printc", Char); ("printf", Float);]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals are void or duplicates *)
    check_binds "formal" func.formals;
    (* TODO: make sure no declarations are void or duplicates *)

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    let symbol_table = Hashtbl.create 1234567

    in

    ignore(List.iter (fun (ty, name) -> Hashtbl.add symbol_table name ty) (globals @ func.formals));

(* 
    (* Build local symbol table of variables for this function *)
    List.iter (*(fun (ty, name) -> Hashtbl.add symbol_table name ty)*) print_endline (globals @ func.formals)
        (* TODO: update symbol table upon seeing a declaration *)
    in *)

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try Hashtbl.find symbol_table s
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        Literal l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Concat (l1, l2) -> (String, SConcat(l1, l2))
      | CharLit l -> (Char, SCharLit l)
      | ArrayIntLit l -> (Array(Int,(List.length l) ), SArrayIntLit l )
      | ArrayIndexLit (var, e) -> let (ty, e') = check_expr e in 
          (match ty with
          | Int ->  (typ_of_array (type_of_identifier var), SArrayIndexLit (var, (ty, e')))
          | _ -> raise (Failure "invalid index type; not an int"))
      | Id var -> (type_of_identifier var, SId var)
      | Noexpr -> (Void, SNoexpr)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Modulo | Mul | Div when t1 = Int -> Int
            | Add | Sub | Mul | Div when t1 = Float -> Float
            | Equal | Neq -> Bool
            | Less | Gre | Leq | Geq when (t1 = Int || t1 = Float) -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> let checked_stmt = check_stmt s in checked_stmt :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | For(e1, e2, e3, st) ->
          SFor(check_expr e1, check_bool_expr e2,check_expr e3, check_stmt st)
      | Declare(ty, s) -> ignore (check_declare ty s); ignore (Hashtbl.add symbol_table s ty); SDeclare (ty, s)
      | DeclareAndAssign(ty, s, e) ->
          ignore (check_declare ty s);
          ignore (Hashtbl.add symbol_table s ty);
          let (t, e') = check_expr e in if (t <> ty) then raise(Failure("type mismatch " ^ string_of_typ t ^ ";" ^ string_of_typ ty ));
          (* TODO: Better error message*)
          SDeclareAndAssign(ty, s, (t,e'))
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)
