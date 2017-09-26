open Core

exception Undefined_symbol of string
exception Type_error of string

let output_byte byte =
    Out_channel.output_binary_int Out_channel.stdout byte

(* TODO: change hardcoded bytecode numbers to constants *)
let rec compile_ast symbol_table syntax =
    match syntax with
    | Ast.Program syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children  
    | Ast.Numeric syntax ->
        output_byte 1;
        output_byte syntax
    | Ast.Expression syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children
    | Ast.Reference syntax ->
        let declaration = SymbolTable.find_symbol syntax symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration ->
                output_byte 6;
                output_byte declaration
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
                output_byte 8;
                output_byte declaration.index;                
                output_byte (Hashtbl.length declaration.parameters.symbols);
                output_byte (Hashtbl.length declaration.locals.symbols);
                compile_ast declaration.locals syntax.code;
                output_byte 9
        ))
    | Ast.FunctionCall syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration -> raise (Type_error "you tried to invoke a regular variable, like a... Like a warlock.")
            | SymbolTable.FunctionDeclaration declaration ->
                List.iter ~f:(compile_ast symbol_table) syntax.children;
                output_byte 10;
                output_byte declaration.index
        ))
    | Ast.ArgumentList syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children
    | Ast.StatementList syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children
    | Ast.ParameterList syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children
    | Ast.VariableDeclaration syntax -> ()
    | Ast.ParamDeclaration syntax -> ()  
    | Ast.VariableAssignment syntax ->
        let declaration = SymbolTable.find_symbol syntax.id symbol_table in
        (match declaration with
        | None -> raise (Undefined_symbol "a reference to a *ghost* variable!")
        | Some declaration -> (
            match declaration with
            | SymbolTable.VariableDeclaration declaration ->
                output_byte 7;
                output_byte declaration
            | SymbolTable.FunctionDeclaration declaration -> raise (Type_error "you can't just reassign function bindings, we live in a society")
        ))
    | Ast.ReturnStatement syntax ->
        List.iter ~f:(compile_ast symbol_table) syntax.children;
        output_byte 11
    | Ast.Statement syntax -> List.iter ~f:(compile_ast symbol_table) syntax.children
    | Ast.Operator syntax -> 
      match syntax with
      | Ast.Multiplication syntax -> output_byte 4 
      | Ast.Addition syntax -> output_byte 2 
      | Ast.Division syntax -> output_byte 5
      | Ast.Subtraction syntax -> output_byte 3
      | Ast.Negation syntax -> output_byte 12 

let filename = Sys.argv.(1)

let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    let ast = Parser.main Lexer.token filebuf in
    let st = (SymbolTable.populate_symbol_table ast) in
    compile_ast st ast
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  | SymbolTable.Error msg ->
      Printf.eprintf "%s%!" msg  
  ;
  close_in input

let _ = main ()