(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Coala" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  and char_t   = L.pointer_type (L.i8_type context)
  and string_t   = L.pointer_type (L.i8_type context) in

  let array_of_int_t = L.array_type i32_t in

  (* Return the LLVM type for a Coala type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> string_t
    | A.Char -> char_t
    | A.Float -> float_t
    | A.Array(A.Int, size) -> array_of_int_t size
    | A.Array(_, _) -> raise (Failure ("illegal array"))
    | A.Void -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init =match t with
      A.Float -> L.const_float (ltype_of_typ t) 0.0
      | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars = Hashtbl.create 1234567 in
      let add_formal (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        Hashtbl.add local_vars n local
      
      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local builder (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in Hashtbl.add local_vars n local_var
      in

      ignore(List.iter2 add_formal fdecl.sformals (Array.to_list (L.params the_function)));

      (* List.fold_left add_local formals fdecl.slocals  *)


    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try Hashtbl.find local_vars n
      with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l 
      | SCharLit s  -> L.build_global_stringptr ((String.sub s 1 ((String.length s) - 2) ) ^ "\n") s builder
    (* TODO: CHECK THIS *)
      | SStringLit s -> L.build_global_stringptr ((String.sub s 1 ((String.length s) - 2) ) ^ "\n") s builder
      | SConcat(s1, s2) -> L.build_global_stringptr ((String.sub s1 1 ((String.length s1) - 2) ) ^ (String.sub s2 1 ((String.length s2) - 2) ^ "\n")) "tmp" builder
      | SArrayIntLit l -> L.const_array i32_t (Array.map (L.const_int i32_t) (Array.of_list l))
      | SArrayIndexLit (var, e) -> let (typ, _) = e in
        (match typ with
        | A.Int -> (*let llv = (L.build_load (lookup var) var builder) in *)
          let index = (build_expr builder e) in
          let gep = (L.build_in_bounds_gep (lookup var) [| L.const_int i32_t 0; index |] "index" builder) in L.build_load gep "array_access" builder 
        | _ -> raise( Failure "Index should be an integer")
        )
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
          (match op with 
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul    -> L.build_fmul
          | A.Div     -> L.build_fdiv 
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Gre -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or | A.Modulo ->
              raise (Failure "internal error: semant should have rejected and/or on float")
          ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mul     -> L.build_mul 
         | A.Div     -> L.build_sdiv 
         | A.Modulo     -> L.build_srem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Gre     -> L.build_icmp L.Icmp.Sgt
        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall ("prints", [e]) ->
            L.build_call printf_func [|(build_expr builder e) |]
              "printf" builder
      | SCall ("printc", [e]) ->
                L.build_call printf_func [|(build_expr builder e) |]
                  "printf" builder
      | SCall ("printf", [e]) -> 
                L.build_call printf_func [| float_format_str ; (build_expr builder e) |]
                  "printf" builder
      | SCall ("printb", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = (match fdecl.srtyp with 
                              A.Void -> ""
                            | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SDeclare (typ, s) -> ignore(add_local builder (typ, s)); builder
      | SDeclareAndAssign (typ, s, e) -> ignore(add_local builder (typ, s)); let e' = build_expr builder e in
      ignore(L.build_store e' (lookup s) builder); builder
      | SReturn e -> ignore(match fdecl.srtyp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (build_expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

      | SFor (e1, e2, e3, body) -> build_stmt builder
        ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    (* add_terminal func_builder (L.build_ret (L.const_int i32_t 0)) *)
    add_terminal func_builder (match fdecl.srtyp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in

  List.iter build_function_body functions;
  the_module
