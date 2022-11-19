let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Coala.Coalaparse.program_rule Coala.Scanner.token lexbuf in
  let sprogram = Coala.Semant.check program in
  print_endline (Coala.Sast.string_of_sprogram sprogram)
