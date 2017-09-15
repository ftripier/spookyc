open Core.Std

let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    Ast.print_ast (Parser.main Lexer.token filebuf) ~level:0
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
  close_in input

let _ = main ()