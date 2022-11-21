(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

   type action = Ast | Sast | LLVM_IR

   let () =
     let action = ref LLVM_IR in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
     ] in
     let usage_msg = "usage: ./microc.native [-a|-s|-l] [file.mc]" in
     let channel = ref stdin in
     Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
   
     let lexbuf = Lexing.from_channel !channel in
   
     let ast = Coala.Coalaparse.program_rule Coala.Scanner.token lexbuf in
     match !action with
       Ast -> print_string (Coala.Ast.string_of_program ast)
     | _ -> let sast = Coala.Semant.check ast in
       match !action with
         Ast     -> ()
       | Sast    -> print_string (Coala.Sast.string_of_sprogram sast)
       | LLVM_IR -> print_string (Llvm.string_of_llmodule (Coala.Codegen.translate sast))
   