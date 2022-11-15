let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Coala.Coalaparse.program_rule Coala.Scanner.token lexbuf in
  print_endline (Coala.Ast.string_of_program program)
