open Core

exception Undefined_symbol of string
exception Type_error of string
exception No_main of string

let add_main_call symbol_table opcodes =
  let main_func_index = (
    match SymbolTable.find_symbol "boo!" symbol_table with
    | None -> raise (No_main "you need to have a function called boo! in your program. That's the point of entry. Sorry, but that's the meme.")
    | Some dec -> (
      match dec with 
        | SymbolTable.VariableDeclaration m -> raise (No_main "you have a global variable called 'boo!'? Yeah we need that to be a function.")
        | SymbolTable.FunctionDeclaration m -> m.index
    )
  ) in
  let main_call = [10; main_func_index] in
  List.append opcodes main_call

(* TODO: change hardcoded bytecode numbers to constants *)
let rec compile_ast symbol_table syntax =
    match syntax with
    | Ast.Program syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Numeric syntax -> [1; syntax]
    | Ast.Expression syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Reference syntax ->
        let declaration = SymbolTable.find_symbol syntax symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> [6; declaration]
            | SymbolTable.FunctionDeclaration declaration -> raise (Type_error "call the police they're using a function without calling it in an expression")
        ))
    | Ast.FunctionDeclaration syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> raise (Type_error "this is actually pretty creepy because this should never ever happen but I guess we thought this function was a variable I don't know what to tell you man")
            | SymbolTable.FunctionDeclaration declaration ->
              let declarations = [8; declaration.index; (Hashtbl.length declaration.parameters.symbols); (Hashtbl.length declaration.locals.symbols)] in
              let code = compile_ast declaration.locals syntax.code in
              List.append (List.append declarations code) [9]
        ))
    | Ast.FunctionCall syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> raise (Type_error "you tried to invoke a regular variable, like a... Like a warlock.")
            | SymbolTable.FunctionDeclaration declaration -> List.append
                (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
                [10; declaration.index]
        ))
    | Ast.ArgumentList syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.StatementList syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.ParameterList syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.VariableDeclaration syntax -> []
    | Ast.ParamDeclaration syntax -> []
    | Ast.VariableAssignment syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> List.append
              (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
              [7; declaration]
            | SymbolTable.FunctionDeclaration declaration -> raise (Type_error "you can't just reassign function bindings, we live in a society")
        ))
    | Ast.ReturnStatement syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [11]
    | Ast.Statement syntax -> List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node))
    | Ast.Operator syntax -> 
      match syntax with
      | Ast.Multiplication syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [4]
      | Ast.Addition syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [2] 
      | Ast.Division syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [5]
      | Ast.Subtraction syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [3]
      | Ast.Negation syntax -> List.append
        (List.fold_left syntax.children ~init:([]: int list) ~f:(fun acc node -> List.append acc (compile_ast symbol_table node)))
        [12]

let compile filename =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    let st = (SymbolTable.populate_symbol_table ast) in
    BytecodeInterpreter.interpret (Stream.of_list (add_main_call st (compile_ast st ast)))
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  | SymbolTable.Error msg ->
      Printf.eprintf "%s%!" msg  
  ;
  close_in input

let spec =
  let open Command.Spec in
  empty
  +> anon("filename" %: file)
  
let command =
  Command.basic
  ~summary:"The Spooky language compiler"
  ~readme:(fun () -> "TODO")
  spec
  (fun filename () -> compile filename)

let () =
  Command.run ~version:"0.10" ~build_info:"RWO" command
