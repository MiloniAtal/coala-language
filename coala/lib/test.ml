open Ast
open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Coalaparse.program_rule Scanner.token lexbuf in
  let sast = Semant.check program in
  print_string (Sast.string_of_sprogram sast)
