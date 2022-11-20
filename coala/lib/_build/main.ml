let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Coalaparse.program_rule Scanner.token lexbuf in
  let sprogram = Semant.check program in
  print_endline (Sast.string_of_sprogram sprogram)
