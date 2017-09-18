open Core

let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    Ast.print_ast ast ~level:0;
    let st = SymbolTable.populate_symbol_table ast in
    SymbolTable.print_table st

  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
  close_in input

let _ = main ()